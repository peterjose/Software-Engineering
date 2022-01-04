package de.uni_saarland.cs.se
package decorator

import scala.annotation.tailrec

/**
 * Interface for the collection components and decorators
 *
 * @tparam A the type of elements in the collection
 */
trait Collection[A] {

  def push(element: A): Boolean

  def pop(): Option[A]

  def peek(): Option[A]

  def size: Int = 0
}

/**
 * Superclass for the concrete components.
 *
 * @param ordering needed for the priority feature so that one can compare values in the collection;
 *                 To properly handle this parameter, subclasses should be declared as such:
 *                   class Foo[A : Ordering] extends AbstractCollection[A] {...}
 * @tparam A the type of elements in the collection
 */
abstract class AbstractCollection[A](implicit val ordering: Ordering[A]) extends Collection[A] {
  protected var elements: List[A] = Nil
}

/**
 *
 *
 * @tparam A the type of elements in the collection
 */
class FIFOCollection[A : Ordering]() extends AbstractCollection[A]{

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

  override def size: Int = {

    elements.size
  }
}

/**
 *
 * @tparam A the type of elements in the collection
 */
class LIFOCollection[A : Ordering]() extends AbstractCollection[A]{


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


  override def size: Int = {
    elements.size
  }

}

/**
 *
 * @tparam A the type of elements in the collection
 */
class PriorityCollection[A : Ordering]() extends AbstractCollection[A]{
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


  override def size: Int = {

    elements.size
  }
}

/**
 *
 * @param capacity
 * @param abstractCollectionP
 * @tparam A the type of elements in the collection
 */
class CapacityDecorator[A : Ordering](val capacity: Int, var abstractCollectionP: Collection[A]) extends Collection [A]{

  override def push(element: A): Boolean={
    if (size >= capacity) {
      return false
    }
    abstractCollectionP.push(element)
  }

  override def pop(): Option[A] = {
    abstractCollectionP.pop()
  }

  override def peek(): Option[A] = {
    abstractCollectionP.peek()
  }

  override def size: Int = abstractCollectionP.size
}

/**
 *
 * @param abstractCollectionP
 * @tparam A the type of elements in the collection
 */
class UniquenessDecorator[A : Ordering](var abstractCollectionP: Collection[A]) extends Collection [A]{
  var ele: List[A] = Nil
  override def push(element:  A): Boolean = {
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
    if (hasElement(element, ele)) {
      return false
    }
    ele = element :: ele
    abstractCollectionP.push(element)
  }

  override def pop(): Option[A] = {
    val value = abstractCollectionP.pop()
    ele = ele.filter(_ != value.get)
    value
  }

  override def peek(): Option[A] = {
    abstractCollectionP.peek()
  }

  override def size: Int = abstractCollectionP.size
}

/**
 *
 * @param abstractCollectionP
 * @tparam A the type of elements in the collection
 */
class LoggingDecorator[A : Ordering](var abstractCollectionP: Collection[A]) extends Collection [A]{

  override def push(element: A): Boolean={
    if( abstractCollectionP.push(element)){
      println(s"Pushing element $element.")
      true
    }else {
      println("Failed to push element.")
      false
    }
  }

  override def pop(): Option[A] = {
    val value = abstractCollectionP.pop()
    if(value.isEmpty){
      println("Collection is empty.")
      return None
    }
    println(s"Popping element ${value.get}.")
    value
  }

  override def peek(): Option[A] = {
    val value = abstractCollectionP.peek()
    if(value.isEmpty){
      println("Collection is empty.")
      return None
    }
    println(s"Peeking element ${value.get}.")
    value
  }

  override def size: Int = {
    println(s"Current size is ${abstractCollectionP.size}.")
    abstractCollectionP.size
  }
}


/* EOF */