package psprepl

import psp.std._, all._, StdShow._

/** These classes are imported into the running repl.
 */
object INREPL {
  /** For some type which psp knows how to deconstruct, you can print all its members
   *  to the console by appending > or >> to the creating expression, depending on whether
   *  you want to require a Show[A] instance.
   */
  implicit class ReplShowWalks[A, R](repr: R)(implicit z: Walks[A, R]) {
    def xs = z walk repr
    def >>(implicit z: Show[A])                      = xs foreach out.println
    def !>(implicit ord: Order[A], z: Show[A]): Unit = xs.sort foreach out.println
  }
  implicit class ReplShowRepr[R](repr: R) {
    def > (implicit z: Show[R] = Show.Inherited) = out.println(repr.pp)
  }
}
