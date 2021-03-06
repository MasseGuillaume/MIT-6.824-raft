package porcEpic

import zio._
import zio.test._
import zio.test.Assertion._

import porcEpic.{fromLong => t}
import OperationKind._

object EntryTest extends DefaultRunnableSpec {
  def spec = suite("EntryTest")(
    test("fromOperations")(


      assert(Entry.fromOperations(history))(equalTo(List(
        Entry.Call   ( value = (W, 0), time = t(0), id = 0, clientId = cid(0)),
        Entry.Return ( value = 0     , time = t(1), id = 0, clientId = cid(0)),
        Entry.Call   ( value = (W, 1), time = t(2), id = 1, clientId = cid(1)),
        Entry.Call   ( value = (R, 1), time = t(3), id = 2, clientId = cid(2)),
        Entry.Call   ( value = (R, 0), time = t(4), id = 3, clientId = cid(3)),
        Entry.Return ( value = 1     , time = t(5), id = 3, clientId = cid(3)),
        Entry.Return ( value = 1     , time = t(6), id = 2, clientId = cid(2)),
        Entry.Return ( value = 1     , time = t(7), id = 1, clientId = cid(1))
      )))
    )
  )
}