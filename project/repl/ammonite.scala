package psp

import std._, all._, api._, StdShow._
import ammonite.repl.{ Ref, Repl, Storage }
import ammonite.repl.Main.defaultAmmoniteHome
import ammonite.repl.frontend.FrontEnd
import java.lang.System

object ReplMain {
  def storage = Storage(defaultAmmoniteHome, None)
  def initImports = sm"""
    |import psp._, std._, all._, api._
    |import StdShow._, INREPL._
    |import Unsafe.promoteIndex
  """

  def main(args: Array[String]): Unit = new REPL(Ref(storage), initImports, args.toVec).start()
}

/** This sets up the ammonite repl with the correct compiler settings
 *  and desired namespace elements and aesthetics.
 */
class REPL(storage: Ref[Storage], initCode: String, scalacArgs: Vec[String]) extends Repl(System.in, System.out, System.err, storage, "", emptyValue) {
  override val frontEnd = Ref[FrontEnd](FrontEnd.JLineUnix)
  override val prompt   = Ref("psp> ")

  private def banner               = s"\npsp-std repl (ammonite $ammoniteVersion, scala $scalaVersion, jvm $javaVersion)"
  override def printBanner(): Unit = printStream println banner

  import interp.replApi._

  def start(): Unit = {
    compiler.settings.processArguments(scalacArgs.to, processAll = true)
    load(initCode)
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
  implicit final class ReplOpsWithShow[A](val xs: View[A]) {
    def > (implicit z: Show[A] = Show.Inherited)     = println(pp"$xs")
    def >>(implicit z: Show[A])                      = println(pp"$xs")
    def !>(implicit ord: Order[A], z: Show[A]): Unit = println(pp"${ xs.sort }")
  }

  implicit def showToAmmonite[A](implicit z: Show[A]): pprint.PPrinter[A] =
    pprint.PPrinter[A]((t, c) => vec(z show t).iterator)
}
