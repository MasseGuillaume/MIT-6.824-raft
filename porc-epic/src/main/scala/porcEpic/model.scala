package porcEpic

opaque type Time = Long
def fromLong(value: Long): Time = value
def toLong(time: Time): Long = time

opaque type ClientId = Int
def cid(value: Int): ClientId = value

case class Operation[S, T](
  clientId: ClientId,
  input: T,
  invocation: Time,
  output: S,
  response: Time
)

sealed trait Entry[S, T] {
  val time: Time
  val id: Int
  val clientId: ClientId

  def withId(id0: Int): Entry[S, T] = {
    this match {
      case c: Entry.Call[_, _]   => c.copy(id = id0)
      case r: Entry.Return[_, _] => r.copy(id = id0)
    }
  }
}

object Entry {
  case class   Call[S, T](value: T, time: Time, id: Int, clientId: ClientId) extends Entry[S, T]
  case class Return[S, T](value: S, time: Time, id: Int, clientId: ClientId) extends Entry[S, T]

  def fromOperations[S, T](history: List[Operation[S, T]]): List[Entry[S, T]] = {
    history.zipWithIndex.flatMap ( (operation, index) =>
      List[Entry[S, T]](
          Call(operation.input,  operation.invocation, index, operation.clientId),
        Return(operation.output, operation.response,   index, operation.clientId)
      )
    ).sorted
  }
}

trait Show[T]:
  def show(a: T): String

trait Eq[T]:
  def equal(a: T, b: T): Boolean

extension [T](a: T)(using e: Eq[T])
  def equal(b: T): Boolean = e.equal(a, b)

trait Model[S, T](using Eq[S], Show[S]){
  def partitionOperations: List[Operation[S, T]] => List[List[Operation[S, T]]] =
    noPartitionOperation

  def partitionEntries: List[Entry[S, T]] => List[List[Entry[S, T]]] =
    noPartitionEntries

  def initial: S

  def step(state: S, input: T, output: S): (Boolean, S)

  def describeOperation(input: T, output: S): String
}

def noPartitionOperation[S, T](history: List[Operation[S, T]]): List[List[Operation[S, T]]] = 
  List(history)

def noPartitionEntries[S, T](history: List[Entry[S, T]]): List[List[Entry[S, T]]] = 
  List(history)

enum CheckResult:
  case Unknown
  case Ok
  case Illgal
