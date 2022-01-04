package de.uni_saarland.cs.se
package traits


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