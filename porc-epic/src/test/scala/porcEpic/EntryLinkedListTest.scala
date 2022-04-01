package porcEpic

import zio._
import zio.test._
import zio.test.Assertion._

object EntryLinkedListTest extends DefaultRunnableSpec {


  

  val entries = Entry.fromOperations(history)
  // println()
  // println()
  // println("---")
  // println()
  // println()
  // entries.foreach(println)
  // println()
  // println()
  // println("---")
  // println()
  // println()
  // println(EntryLinkedList(entries))
  // println()
  // println()


  // val list = EntryLinkedList(0, 0)

  def spec = suite("EntryLinkedList")(
    test("insertBefore")(
      assert(1)(equalTo(1))
    ),

    // test("toString")(
    //   assert(EntryLinkedList(entries))(equalTo(
    //     """|EntryLinkedList(
    //        |  ∅,
    //        |  (0)[value = , matches = ∅],
    //        |  (0)[value = , matches = ∅],
    //        |  (0)[value = , matches = ∅],
    //        |  (0)[value = , matches = ∅],
    //        |  (0)[value = , matches = ∅],
    //        |  (0)[value = , matches = ∅],
    //        |  (0)[value = , matches = ∅],
    //        |  (0)[value = , matches = ∅],
    //        |  ∅
    //        |)""".stripMargin
        
    //   ))
    // ),

    // test("length")(
    //   assert(EntryLinkedList(entries).length)(equalTo(8))
    // ),
    // test("length")(
    //   assert(1)(equalTo(1))
    // ),
    
    // test("lift")(
    //   assert(1)(equalTo(1))
    // ),
    // test("unlift")(
    //   assert(1)(equalTo(1))
    // )
  )
}
