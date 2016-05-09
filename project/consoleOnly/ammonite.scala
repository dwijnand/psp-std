package psp

import std._, all._, api._, StdShow._
import ammonite.repl.{ Ref, Repl, Storage }
import ammonite.repl.Main.defaultAmmoniteHome
import ammonite.repl.frontend.FrontEnd
import java.lang.System

object ReplMain {
  def storage = Ref(Storage(defaultAmmoniteHome, None))
  def initImports = sm"""
    |import psp._, std._, all._, api._
    |import StdShow._, INREPL._
    |import Unsafe.promoteIndex
  """
  // Working around ammonite bugs.
  // https://github.com/lihaoyi/Ammonite/issues/213
  def workarounds = {
    def mkNames(name: String, variance: String): String = s"type $name[${variance}A] = psp.api.$name[A] ; val $name = psp.std.$name"
    def cov         = vec("Order", "Eq", "Show") map (mkNames(_, "-"))
    def inv         = vec("Empty") map (mkNames(_, ""))
    cov ++ inv joinLines
  }

  def main(args: Array[String]): Unit = REPL.start(args: _*)
}

trait ReplOverrides extends Repl {
  private def banner = s"\npsp-std repl (ammonite $ammoniteVersion, scala $scalaVersion, jvm $javaVersion)"
  override val frontEnd = Ref[FrontEnd](FrontEnd.JLineUnix)
  override val prompt   = Ref("psp> ")

  override def printBanner(): Unit = printStream println banner
}

import ReplMain._

/** This sets up the ammonite repl with the correct compiler settings
 *  and desired namespace elements and aesthetics.
 */
object REPL extends Repl(System.in, System.out, System.err, storage, "", Nil) with ReplOverrides {
  import interp.replApi._

  def start(args: String*): Unit = {
    compiler.settings.processArguments(args.toList, processAll = true)
    load(initImports)
    load(workarounds)
    run()
  }
  override def action() = {
    val res = super.action()
    printStream.println("") // Blank line between results.
    res
  }
}

/** These classes are imported into the running repl.
 */
object INREPL {
  /** For some type which psp knows how to deconstruct, you can print all its members
   *  to the console by appending > or >> to the creating expression, depending on whether
   *  you want to require a Show[A] instance.
   */
  implicit final class ReplOpsWithShow[A, R](val xs: R)(implicit val z: UnbuildsAs[A, R]) {
    private def run(f: Each[A] => Each[String]): R = sideEffect(xs, f(Each(z unbuild xs foreach _)) foreach (x => println(x)))

    def > (implicit z: Show[A] = inheritShow)        = run(_ map z.show)
    def >>(implicit z: Show[A])                      = run(_ map z.show)
    def !>(implicit ord: Order[A], z: Show[A]): Unit = run(_.m.sorted map z.show)
  }

  implicit def showToAmmonite[A](implicit z: Show[A]): pprint.PPrinter[A] =
    pprint.PPrinter[A]((t, c) => BiIterator[String](vec(z show t)))
}
