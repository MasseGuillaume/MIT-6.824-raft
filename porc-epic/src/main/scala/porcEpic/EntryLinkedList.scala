package porcEpic

import scala.collection.mutable.StringBuilder

type EntryLinkedList[T] = DoubleLinkedList[EntryNode[T]]

object EntryLinkedList {
  def apply[T](entries: List[Entry[T]]): DoubleLinkedList[EntryNode[T]] = {
    var root: DoubleLinkedList[EntryNode[T]] = null
    val matches = collection.mutable.Map.empty[Int, DoubleLinkedList[EntryNode[T]]]

    entries.reverse.foreach{ elem => 
      val entry = 
        elem.kind match {
          case EntryKind.Call => 
            val entry = DoubleLinkedList(EntryNode(elem.value, elem.id))
            matches(elem.id) = entry
            entry
            
          case EntryKind.Return =>
            DoubleLinkedList(EntryNode(elem.value, elem.id, matches.getOrElse(elem.id, null)))
        }

      entry.insertBefore(root)
      root = entry

      println(matches)
      println(root)
    }

    root
  }
}

class DoubleLinkedList[T](
  val elem: T, 
  var prev: DoubleLinkedList[T] = null,
  var next: DoubleLinkedList[T] = null
) {

  def length: Int = {
    var l = 1
    var n = this
    while (n.next != null) {
      n = n.next
      l += 1
    }
    l
  }

  def insertBefore(mark: DoubleLinkedList[T]): this.type = {
    if (mark != null) {
      val beforeMark = mark.prev
      mark.prev = this
      this.next = mark
      if (beforeMark != null) {
        this.prev = beforeMark
        beforeMark.next = this
      }
    }
    this
  }
}

/**
 * @param matches:  if it's a call it points to the return entry
 */
case class EntryNode[T](
  value: T,
  id: Int,
  matches: DoubleLinkedList[EntryNode[T]] = null
)

extension [T](list: DoubleLinkedList[EntryNode[T]]) {

  def lift(): Unit = {
    import list._
    prev.next = next
    next.prev = prev
    elem.matches.prev.next = elem.matches.next
    if (elem.matches.next != null) {
      elem.matches.next.prev = elem.matches.prev
    }
  }

  def unlift(): Unit = {
    import list._
    elem.matches.prev.next = elem.matches
    if (elem.matches.next != null) {
      elem.matches.next.prev = elem.matches
    }
    prev.next = list
    next.prev = list
  }

  def show: String = {
    import list._

    val builder = new StringBuilder("EntryLinkedList(\n")

    if (prev != null) {
      builder ++= "  ...,\n"
    } else {
      builder ++= "  ∅,\n"
    }

    def show(that: DoubleLinkedList[EntryNode[T]]): Unit = {
      builder ++= s"  (${elem.id})["
      builder ++= "value = "
      if (that.elem.value != null) {
        builder ++= that.elem.value.toString
      } else {
        builder ++= "∅"
      }

      if (that.elem.matches != null) {
        builder ++= ", matches = " + that.elem.matches.elem.id.toString
      }

      builder ++= "],\n"
    }

    var current = list
    show(current)
    while (current.next != null) {
      current = current.next
      show(current)
    }
    

    builder ++= "  ∅\n)"
    
    builder.toString
  }
}
