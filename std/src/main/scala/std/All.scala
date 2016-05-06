package psp
package std

/** One import which includes the implicits, one which doesn't.
 *  This choice is mutually exclusive: everything which is in exp is in all.
 */
object exp extends AllExplicit
object all extends AllExplicit with AllImplicit
