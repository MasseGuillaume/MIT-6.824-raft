package porcEpic

import zio._
import scala.collection.mutable.{BitSet => MBitset, Map => MMap}

enum Verbosity:
  case Debug
  case Error

extension [S, T](model: Model[S, T])(using eq: Eq[S]) {

  def checkOperations(
    history: List[Operation[S, T]],
    timeout: Option[Duration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[S, T]) = {
    val partitions =
      model.partitionOperations(history).map(
        Entry.fromOperations
      )

    checkParallel(partitions, timeout, verbosity)
  }

  def checkEntries(
    history: List[Entry[S, T]],
    timeout: Option[Duration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[S, T]) = {
    val partitions =  model.partitionEntries(history).map(renumber)
    checkParallel(partitions, timeout, verbosity)
  }

  private def checkParallel(
    history: List[List[Entry[S, T]]],
    timeout: Option[Duration],
    verbosity: Verbosity
  ): (CheckResult, LinearizationInfo[S, T]) = {
    val (ok, l) = checkSingle(history.head)
    
    val result = 
      if (ok) CheckResult.Ok
      else CheckResult.Illgal

    (result, null)
  }

  private def checkSingle(history: List[Entry[S, T]]): (Boolean, Array[Array[Int]]) = {
    case class CacheEntry(linearized: MBitset, state: S)
    case class CallEntry[T](entry: EntryLinkedList[S, T], state: S)

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
    var state = model.initial
       
    val headEntry = entry.insertBefore(
      DoubleLinkedList[EntryNode[S, T]](
        EntryNode.Return[S, T](
          value = null.asInstanceOf[S],
          id = -1
        )
      )
    )

    while (headEntry.next != null) {
      entry.elem match {
        case node: EntryNode.Call[_, _] =>
          val matching = 
            node.matches.elem match {
              case r: EntryNode.Return[_, _] => r.asInstanceOf[EntryNode.Return[S, T]]
              case _: EntryNode.Call[_, _]   => throw new Exception("call matching should be a return")
            }

          val (ok, newState) = model.step(state, node.value, matching.value)
          if (ok) {
            val newLinearized = linearized.clone().set(node.id)
            val newCacheEntry = CacheEntry(newLinearized, newState)
            if (!cacheContains(newCacheEntry)) {
              val hash = newLinearized.hashCode
              cache(hash) = newCacheEntry :: cache(hash)
              calls = CallEntry(entry, state) :: calls
              state = newState
              linearized.set(node.id)
              entry.lift()
              entry = headEntry.next
            } else {
              entry = entry.next
            }
          } else {
            entry = entry.next
          }

        case r: EntryNode.Return[_, _] =>
          val callsLength = calls.length
          if (callsLength == 0) {
            return (false, longest)
          }
          var seq: Array[Int] = null
          calls.reverse.foreach { v =>
            if (longest(v.entry.elem.id) == null || 
                callsLength > longest(v.entry.elem.id).length) {
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


def renumber[S, T](events: List[Entry[S, T]]): List[Entry[S, T]] = {
  val renumbering = MMap.empty[Int, Int]
  var id = 0

  events.map{ event =>
    event.withId(
      renumbering.getOrElse(event.id, {
        val i = id
        renumbering(event.id) = id
        id += 1
        i
      })
    )
  }
}

given EntryOrderingByTime[S, T]: Ordering[Entry[S, T]] =
  Ordering.by(e =>
    (
      toLong(e.time),
      e match {
        case _: Entry.Call[_, _] => 0
        case _: Entry.Return[_, _] => 1
      }
    )
  )

case class LinearizationInfo[S, T](
  history: List[List[Entry[S, T]]],
  partialLinearizations: List[List[List[Int]]]
)
