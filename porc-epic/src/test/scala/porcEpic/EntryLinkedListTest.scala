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
           |    (0)[value = (W,0), matches = 0],
           |    (0)[value = 0],
           |    (1)[value = (W,1), matches = 1],
           |    (2)[value = (R,1), matches = 2],
           |    (3)[value = (R,0), matches = 3],
           |    (3)[value = 1],
           |    (2)[value = 1],
           |    (1)[value = 1],
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
