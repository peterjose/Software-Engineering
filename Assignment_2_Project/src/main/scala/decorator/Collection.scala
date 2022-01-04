package de.uni_saarland.cs.se
package decorator


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


// TODO: implement task 1b