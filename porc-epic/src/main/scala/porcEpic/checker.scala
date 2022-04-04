package porcEpic

import zio._
import scala.collection.mutable.{BitSet => MBitset, Map => MMap}

enum Verbosity:
  case Debug
  case Error

extension [S, T](model: Model[S, T])(using eq: Eq[S]) {

  def checkOperations(
    history: List[Operation[T]],
    timeout: Option[Duration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[T]) = {
    val partitions =
      model.partitionOperations(history).map(
        Entry.fromOperations
      )

    checkParallel(partitions, timeout, verbosity)
  }

  def checkEvents(
    history: List[Event[T]],
    timeout: Option[Duration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[T]) = {
    val partitions = 
      model.partitionEvents(history).map(events =>
        Entry.fromEvents(renumber(events))
      )

    checkParallel(partitions, timeout, verbosity)
  }

  private def checkParallel(
    history: List[List[Entry[T]]],
    timeout: Option[Duration],
    verbosity: Verbosity
  ): (CheckResult, LinearizationInfo[T]) = {
    val (ok, l) = checkSingle(history.head)
    
    val result = 
      if (ok) CheckResult.Ok
      else CheckResult.Illgal

    (result, null)
  }

  private def checkSingle(history: List[Entry[T]]): (Boolean, Array[Array[Int]]) = {
    case class CacheEntry(
      linearized: MBitset,
      state: S
    )

    case class CallEntry[T](
      entry: EntryLinkedList[T],
      state: S
    )

    extension (bitset: MBitset) {
      def set(v: Int): MBitset = bitset += v
      def clear(v: Int): MBitset = bitset -= v
    }
    
    var entry = EntryLinkedList(history)
    val n = entry.length / 2
    val linearized = MBitset.fromBitMaskNoCopy(Array.ofDim(n))
    val cache = MMap.empty[Int, List[CacheEntry]].withDefaultValue(Nil)

    def cacheContains(entry: CacheEntry): Boolean = {
      cache.getOrElse(entry.linearized.hashCode, Nil).exists( elem =>
        entry.linearized == elem.linearized && 
          entry.state.equal(elem.state)
      )
    }

    var calls = List.empty[CallEntry[T]]
    val longest = Array.ofDim[Array[Int]](n)
    var state = model.initial()
        
    val bogus = DoubleLinkedList[EntryNode[T]](
      EntryNode[T](
        value = null.asInstanceOf[T],
        id = -1
      )
    )
    val headEntry = entry.insertBefore(bogus)

    while (headEntry.next != null) {
      if (entry.elem.matches != null) {
        val matching = entry.elem.matches
        val (ok, newState) = model.step(state, entry.elem.value, matching.elem.value)
        if (ok) {
          val newLinearized = linearized.clone().set(entry.elem.id)
          val newCacheEntry = CacheEntry(newLinearized, newState)
          if (!cacheContains(newCacheEntry)) {
            val hash = newLinearized.hashCode
            cache(hash) = newCacheEntry :: cache(hash)
            calls = CallEntry(entry, state) :: calls
            state = newState
            linearized.set(entry.elem.id)
            entry.lift()
            entry = headEntry.next
          } else {
            entry = entry.next
          }
        } else {
          entry = entry.next
        }
      } else {
        val callsLength = calls.length
        if (callsLength == 0) {
          return (false, longest)
        }
        var seq: Array[Int] = null
        calls.reverse.foreach { v =>
          if (longest(v.entry.elem.id) == null || callsLength > longest(v.entry.elem.id).length) {
            if (seq == null) {
              seq = Array.ofDim(callsLength)
              calls.reverse.zipWithIndex.foreach{(c, i) =>
                seq(i) = v.entry.elem.id
              }
            }
            longest(v.entry.elem.id) = seq
          }
        }
        val callTop = calls.head
        entry = callTop.entry
        val state = callTop.state
        linearized.clear(entry.elem.id)
        calls = calls.tail
        entry.unlift()
        entry = entry.next
      }
    } // while

    val seq = Array.ofDim[Int](calls.length)
    calls.zipWithIndex.reverse.foreach{  (v, i) =>
      seq(i) = v.entry.elem.id
    }

    {
      var i = 0
      while(i < n) {
        longest(i) = seq
        i += 1
      }
    }

    (true, longest)
  }
}

def renumber[T](events: List[Event[T]]): List[Event[T]] = {
  val renumbering = MMap.empty[Int, Int]
  var id = 0

  events.map{ event =>
    event.copy(id = 
      renumbering.getOrElse(event.id, {
        val i = id
        renumbering(event.id) = id
        id += 1
        i
      })
    )
  }
}

enum EntryKind:
  case Call
  case Return

case class Entry[E] (
  kind: EntryKind,
  value: E,
  time: Time,

  id: Int,
  clientId: ClientId,
)

given EntryOrderingByTime[T]: Ordering[Entry[T]] =
  Ordering.by(e => (toLong(e.time), e.kind))

given EntryKindOredring: Ordering[EntryKind] with
  private def toInt(x: EntryKind): Int = 
    x match {
      case EntryKind.Call => 0
      case EntryKind.Return => 1
    }

  def compare(x: EntryKind, y: EntryKind): Int =
    toInt(x) - toInt(y)

object Entry {

  def fromEvents[T](events: List[Event[T]]): List[Entry[T]] = {
    events.map{ event =>
      val entryKind = 
        event.kind match {
          case EventKind.Return => EntryKind.Return
          case EventKind.Call   => EntryKind.Call
        }
        
      Entry(
        kind = entryKind,
        value = event.value,
        time = fromLong(event.id.toLong),
        id = event.id,
        clientId = event.clientId
      )
    }
  }

  def fromOperations[T](history: List[Operation[T]]): List[Entry[T]] = {
    history.zipWithIndex.flatMap { (operation, index) =>
      List[Entry[T]](
        Entry(EntryKind.Call,   operation.input,  operation.invocation, index, operation.clientId),
        Entry(EntryKind.Return, operation.output, operation.response,   index, operation.clientId)
      )
    }.sorted
  }
}

case class LinearizationInfo[T](
  history: List[List[Entry[T]]],
  partialLinearizations: List[List[List[Int]]]
)
