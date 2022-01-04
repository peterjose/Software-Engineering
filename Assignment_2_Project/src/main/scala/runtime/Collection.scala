package de.uni_saarland.cs.se
package runtime

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
}
