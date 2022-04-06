package porcEpic

import zio._
import zio.test._
import zio.test.Assertion._

object EntryLinkedListTest extends DefaultRunnableSpec {

  val entries = Entry.fromOperations(history)

  def spec = suite("EntryLinkedList")(
    test("toString")(
      assert(DoubleLinkedList(1, 2, 3).toString)(equalTo(
        """|DoubleLinkedList(
           |  1,
           |  2,
           |  3,
           |)""".stripMargin
      ))
    ),
    test("EntryLinkedList")(
      assert(EntryLinkedList(entries).toString)(equalTo(
        """|DoubleLinkedList(
           |  Call(0, (W,0), 0),
           |  Return(0, 0),
           |  Call(1, (W,1), 1),
           |  Call(2, (R,1), 2),
           |  Call(3, (R,0), 3),
           |  Return(3, 1),
           |  Return(2, 1),
           |  Return(1, 1),
           |)""".stripMargin
      ))
    ),
    test("length")(
      assert(EntryLinkedList(entries).length)(equalTo(8))
    ),
    // test("lift")(
    //   assert(1)(equalTo(1))
    // ),
    // test("unlift")(
    //   assert(1)(equalTo(1))
    // )
  )
}
