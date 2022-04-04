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
          case EntryKind.Return => 
            val entry = DoubleLinkedList(EntryNode(elem.value, elem.id))
            matches(elem.id) = entry
            entry
            
          case EntryKind.Call =>
            DoubleLinkedList(EntryNode(elem.value, elem.id, matches.getOrElse(elem.id, null)))
        }

      entry.insertBefore(root)
      root = entry
    }

    root
  }
}

object DoubleLinkedList {
  def apply[T](xs: T*): DoubleLinkedList[T] = {
    xs.reverse.foldLeft(null: DoubleLinkedList[T])((acc, x) =>
      (new DoubleLinkedList(x)).insertBefore(acc)
    )
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

  override def toString: String = {
    val builder = new StringBuilder("DoubleLinkedList(\n")

    if (prev != null) {
      builder ++= "  ...,\n"
    } else {
    }

    def show(that: DoubleLinkedList[T]): Unit = {
      builder ++= s"  ${that.elem.toString},\n"
    }

    var current = this
    show(current)
    while (current.next != null) {
      current = current.next
      show(current)
    }
    builder ++= ")"
    builder.toString
  }
}

/**
 * @param matches:  if it's a call it points to the return entry
 */
case class EntryNode[T](
  value: T,
  id: Int,
  matches: DoubleLinkedList[EntryNode[T]] = null
) {
  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= s"  ($id)["
    builder ++= "value = "
    if (value != null) {
      builder ++= value.toString
    } else {
      builder ++= "∅"
    }
    if (matches != null) {
      builder ++= ", matches = " + matches.elem.id.toString
    }
    builder ++= "]"

    builder.toString
  }
}

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
}
