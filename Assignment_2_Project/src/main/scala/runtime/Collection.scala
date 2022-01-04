package de.uni_saarland.cs.se
package runtime

import scala.annotation.tailrec

abstract class CollectionAccess

case class FIFO() extends CollectionAccess

case class LIFO() extends CollectionAccess

case class PRIORITY() extends CollectionAccess


/**
 * Configuration object for the collection class.
 *
 * @param access the type of access to the collection
 * @param capacity the capacity of the collection or `None` for no capacity
 * @param uniqueness whether inserted elements need to be unique
 * @param logging whether to log function calls
 */
class CollectionConfig(
                        val access: CollectionAccess,
                        val capacity: Option[Int] = None,
                        val uniqueness: Boolean = false,
                        val logging: Boolean = false,
                      ) {}


/**
 * A runtime-configurable version of our collection SPL.
 *
 * @param config the configuration for the collection
 * @param ordering needed for the priority feature so that one can compare values in the collection
 * @tparam A the type of elements in the collection
 */
class Collection[A](val config: CollectionConfig)(implicit val ordering: Ordering[A]) {
  // TODO: implement task 1a
  private var elements: List[A] = Nil

  def push(element: A): Boolean = {
    if (config.capacity.isDefined) {
      if (size >= config.capacity.get) {
        if (config.logging)
          println("Failed to push element.")
        return false
      }
    }

    if (config.uniqueness) {
      @tailrec
      def hasElement(e: A, l: List[A]): Boolean = {
        l match {
          case Nil => false
          case list =>
            if (e == list.head) {
              true
            } else {
              hasElement(e, list.tail)
            }
        }
      }
      if (hasElement(element, elements)) {
        if (config.logging)
          println("Failed to push element.")
        return false
      }
    }

    if (config.logging)
      println(s"Pushing element $element.")

    if (config.access == FIFO() || config.access == LIFO()){
      elements = element :: elements
    }else if(config.access == PRIORITY()) {
      def insert(e: A, l: List[A]): List[A] = {
        l match {
          case ::(head, next) =>
            if (ordering.lteq(e, head)) {
              element :: l
            } else {
              head :: insert(e, next)
            }
          case Nil => e :: Nil
        }
      }
      elements = insert(element, elements)
    }
    true
  }

  def pop(): Option[A] = {
    if (elements == Nil) {
      if(config.logging)
        println("Collection is empty.")
      return None
    }
    var head = elements.head
    if(config.access == FIFO()) {
      head = elements.reverse.head
      elements = elements.reverse.tail.reverse
    }
    else if(config.access == LIFO() || config.access == PRIORITY()) {
      head = elements.head
      elements = elements.tail
    }
    if(config.logging)
      println(s"Popping element $head.")

    Some(head)
  }

  def peek(): Option[A] = {
    if (elements == Nil) {
      if (config.logging)
        println("Collection is empty.")
      return None
    }
    var head = elements.head
    if (config.access == FIFO()) {
      head = elements.reverse.head
    }else if (config.access == LIFO() || config.access == PRIORITY()) {
      head = elements.head
    }

   if(config.logging)
      println(s"Peeking element $head.")

    Some(head)
  }


  def size: Int = {
    if(config.logging) {
      println(s"Current size is ${elements.size}.")
    }
    elements.size
  }
}
