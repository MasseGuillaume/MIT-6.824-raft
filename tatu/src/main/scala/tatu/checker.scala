package tatu

import zio.Duration

enum Verbosity:
  case Debug
  case Error

extension [E, S, I, O](model: Model[E, S, I, O]) {

  def checkOperations(
    history: List[Operation[I, O]],
    timeout: Option[Duration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[I | O]) = {
    val partitions =
      model.partitionOperations(history).map(
        Entry.fromOperations
      )

    checkParallel(partitions, timeout, verbosity)
  }

  def checkEvents(
    history: List[Event[E]],
    timeout: Option[Duration] = None,
    verbosity: Verbosity = Verbosity.Error
  ): (CheckResult, LinearizationInfo[E]) = {
    val partitions = 
      model.partitionEvents(history).map(events =>
        Entry.fromEvents(renumber(events))
      )

    checkParallel(partitions, timeout, verbosity)
  }

  private def checkParallel[T](
    history: List[List[Entry[T]]],
    timeout: Option[Duration],
    verbosity: Verbosity
  ): (CheckResult, LinearizationInfo[T]) = {
    ???
  }

  private def checkSingle[T](history: List[Entry[T]]): Boolean = {
    ???
  }
}

def renumber[T](events: List[Event[T]]): List[Event[T]] = {
  val renumbering = collection.mutable.Map.empty[Int, Int]
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

  def fromEvents[E](events: List[Event[E]]): List[Entry[E]] = {
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

  def fromOperations[I, O](history: List[Operation[I, O]]): List[Entry[I | O]] = {
    history.zipWithIndex.flatMap { (operation, index) =>
      List[Entry[I | O]](
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
