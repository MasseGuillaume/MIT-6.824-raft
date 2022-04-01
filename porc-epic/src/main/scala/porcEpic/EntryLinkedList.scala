package porcEpic

import scala.collection.mutable.StringBuilder

object EntryLinkedList {
  def apply[T](entries: List[Entry[T]]): EntryLinkedList[T] = {
    var root: EntryLinkedList[T] = null
    val matches = collection.mutable.Map.empty[Int, EntryLinkedList[T]]

    entries.reverse.foreach{ elem => 
      val entry = 
        elem.kind match {
          case EntryKind.Call => 
            val entry = EntryLinkedList(elem.value, elem.id, null)
            matches(elem.id) = entry
            entry
            
          case EntryKind.Return =>
            EntryLinkedList(elem.value, elem.id, matches.getOrElse(elem.id, null))
        }

      entry.insertBefore(root)
      root = entry
    }

    root
  }
}

case class EntryLinkedList[T](
  value: T,
  id: Int,
  matches: EntryLinkedList[T], // if it's a call it points to the return entry
  var next: EntryLinkedList[T] = null,
  var prev: EntryLinkedList[T] = null
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

  def insertBefore(mark: EntryLinkedList[T]): EntryLinkedList[T] = {
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

  def lift(): Unit = {
    prev.next = next
    next.prev = prev
    matches.prev.next = matches.next
    if (matches.next != null) {
      matches.next.prev = matches.prev
    }
  }

  def unlift(): Unit = {
    matches.prev.next = matches
    if (matches.next != null) {
      matches.next.prev = matches
    }
    prev.next = this
    next.prev = this
  }

  override def toString: String = {
    val builder = new StringBuilder("EntryLinkedList(\n")

    if (prev != null) {
      builder ++= "  ...,\n"
    } else {
      builder ++= "  ∅,\n"
    }

    def show(that: EntryLinkedList[T]): Unit = {
      builder ++= s"  ($id)["
      builder ++= "value = "
      if (that.value != null) {
        builder ++ that.value.toString
      } else {
        builder ++= "∅"
      }

      builder ++= ", "

      builder ++= "matches = "
      if (that.matches != null) {
        builder ++= that.matches.id.toString
      } else {
        builder ++= "∅"
      }

      builder ++= "],\n"
    }

    var current = this
    show(current)
    while (current.next != null) {
      current = current.next
      show(current)
    }
    

    builder ++= "  ∅\n)"
    
    builder.toString
  }
}