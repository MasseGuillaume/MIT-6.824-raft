package porcEpic

opaque type Time = Long
def fromLong(value: Long): Time = value
def toLong(time: Time): Long = time

opaque type ClientId = Int
def cid(value: Int): ClientId = value

case class Operation[T](
  clientId: ClientId,
  input: T,
  invocation: Time,
  output: T,
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
  def equal(a: T, b: T): Boolean

extension [T](a: T)(using e: Eq[T])
  def equal(b: T): Boolean = e.equal(a, b)

class Model[S, T](using Eq[S], Show[T]){
  def partitionOperations: List[Operation[T]] => List[List[Operation[T]]] =
    noPartitionOperation

  def partitionEvents: List[Event[T]] => List[List[Event[T]]] =
    noPartitionEvents

  def initial: S

  def step(state: S, input: T, output: S): (Boolean, S)

  def describeOperation(input: T, output: S): String
}

def noPartitionOperation[T](history: List[Operation[T]]): List[List[Operation[T]]] = 
  List(history)

def noPartitionEvents[T](history: List[Event[T]]): List[List[Event[T]]] = 
  List(history)

enum CheckResult:
  case Unknown
  case Ok
  case Illgal
