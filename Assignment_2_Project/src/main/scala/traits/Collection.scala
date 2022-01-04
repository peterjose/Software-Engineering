package de.uni_saarland.cs.se
package traits

import scala.annotation.tailrec


/**
 * Interface for the collection implementations and traits
 *
 * Keep in mind that the priority feature needs an implicit parameter for the `Ordering` object:
 *   class Foo[A](implicit val ordering: Ordering[A]) extends Collection[A]
 *
 * @tparam A the type of elements in the collection
 */
trait Collection[A] {
  def push(element: A): Boolean

  def pop(): Option[A]

  def peek(): Option[A]

  def size: Int = 0
}

// TODO: implement task 1c

//abstract class A {
//  val message: String
//}
//class B extends A {
//  val message = "I'm an instance of class B"
//}
//trait C extends A {
//  def loudMessage = message.toUpperCase()
//}
//class D extends B with C

/**
 *
 * @tparam A the type of elements in the collection
 */
abstract class AbstractCollection[A] extends Collection[A] {
  protected var elements: List[A] = Nil

  override def size: Int = {
    elements.size
  }
}

/**
 *
 * @tparam A the type of elements in the collection
 */
class FIFOCollection[A] extends AbstractCollection[A]{
//  protected var elements: List[A] = Nil
  override def push(element: A): Boolean = {

    elements = element :: elements

    true
  }

  override def pop(): Option[A] = {
    if (elements == Nil) {

      return None
    }
    val head = elements.reverse.head
    elements = elements.reverse.tail.reverse

    Some(head)
  }

  override def peek(): Option[A] = {
    if (elements == Nil) {
      return None
    }

    Some(elements.reverse.head)
  }

}

/**
 *
 * @tparam A the type of elements in the collection
 */
class LIFOCollection[A] extends AbstractCollection[A]{

//  protected var elements: List[A] = Nil

  override def push(element: A): Boolean = {

    elements = element :: elements
    true
  }

  override def pop(): Option[A] = {
    if (elements == Nil) {

      return None
    }
    val head = elements.head
    elements = elements.tail

    Some(head)
  }

  override def peek(): Option[A] = {
    if (elements == Nil) {

      return None
    }

    Some(elements.head)
  }

}

/**
 *
 * @tparam A the type of elements in the collection
 */
class PriorityCollection[A](implicit val ordering: Ordering[A]) extends AbstractCollection[A]{

//  protected var elements: List[A] = Nil
  override def push(element: A): Boolean = {

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

    true
  }

  override def pop(): Option[A] = {
    if (elements == Nil) {

      return None
    }
    val head = elements.head
    elements = elements.tail

    Some(head)
  }

  override def peek(): Option[A] = {
    if (elements == Nil) {

      return None
    }

    Some(elements.head)
  }

}

/**
 *
 * @tparam A the type of elements in the collection
 */
trait Capacity[A] extends Collection [A]{
  val capacity: Int
  abstract override def push(element: A): Boolean={
    if (size >= capacity) {
      return false
    }
    super.push(element)
  }
}

/**
 *
 * @tparam A the type of elements in the collection
 */
trait Uniqueness[A] extends AbstractCollection [A]{
  abstract override def push(element:  A): Boolean = {
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
      return false
    }
    super.push(element)
  }

  abstract override def pop(): Option[A] = {
    super.pop()
  }
}

/**
 *
 * @tparam A the type of elements in the collection
 */
trait Logging[A] extends Collection [A]{
  abstract override def push(element: A): Boolean={
    if( super.push(element)){
      println(s"Pushing element $element.")
      true
    }else {
      println("Failed to push element.")
      false
    }
  }

  abstract override def pop(): Option[A] = {
    val value = super.pop()
    if(value.isEmpty){
      println("Collection is empty.")
      return None
    }
    println(s"Popping element ${value.get}.")
    value
  }

  abstract override def peek(): Option[A] = {
    val value = super.peek()
    if(value.isEmpty){
      println("Collection is empty.")
      return None
    }
    println(s"Peeking element ${value.get}.")
    value
  }

  abstract override def size: Int = {
    println(s"Current size is ${super.size}.")
    super.size
  }
}

/* EOF */