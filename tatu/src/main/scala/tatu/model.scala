package tatu

opaque type Time = Long
def fromLong(value: Long): Time = value
def toLong(time: Time): Long = time

opaque type ClientId = Int

case class Operation[I, O](
  clientId: ClientId,
  input: I,
  invocation: Time,
  output: O,
  response: Time
)

enum EventKind:
  case Call
  case Return

case class Event[T](
  clientId: ClientId,
  kind: EventKind,
  value: T,
  id: Int
)

trait Show[T]:
  def show: String

trait Eq[T]:
  def eq(a: T, b: T): Boolean

case class Model[E, S, I, O](
  partitionOperations: List[Operation[I, O]] => List[List[Operation[I, O]]],
  partitionEvents: List[Event[E]] => List[List[Event[E]]],
  initial: () => S,
  step: (S, I, O) => (Boolean, S),
)(using Eq[S]) // TODO figure out what we need to show: Show[O], Show[S] 

def noPartition[I, O](history: List[Operation[I, O]]): List[List[Operation[I, O]]] = 
  List(history)

def noPartitionEvent[E](history: List[Event[E]]): List[List[Event[E]]] = 
  List(history)

enum CheckResult:
  case Unknown
  case Ok
  case Illgal

