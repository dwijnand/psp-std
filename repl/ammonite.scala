package psprepl

import psp.std._, all._, StdShow._
import ammonite.repl.{ Ref, Repl, Storage }
import ammonite.repl.Main.defaultAmmoniteHome
import ammonite.repl.frontend.FrontEnd
import java.lang.System

object Main {
  def storage = new Storage.Folder(defaultAmmoniteHome)
  def initImports = sm"""
    |import psp.std._, all._, StdShow._
    |import psprepl._, INREPL._
  """

  def ammoniteVersion = ammonite.Constants.version
  def scalaVersion    = scala.util.Properties.versionNumberString
  def javaVersion     = System.getProperty("java.version")
  def banner          = s"\npsp-std repl (ammonite $ammoniteVersion, scala $scalaVersion, jvm $javaVersion)"

  def main(args: Array[String]): Unit = new REPL(storage, initImports, args.toVec).start()
}

/** This sets up the ammonite repl with the correct compiler settings
 *  and desired namespace elements and aesthetics.
 */
class REPL(storage: Storage, initCode: String, scalacArgs: Vec[String])
    extends Repl(System.in, System.out, System.err, storage, "", ammonite.ops.cwd, Some(Main.banner), emptyValue) {
  override val frontEnd = Ref[FrontEnd](FrontEnd.JLineUnix)
  override val prompt   = Ref("psp> ")

  import interp.replApi._

  def start(): Unit = {
    compiler.settings.processArguments(scalacArgs.to, processAll = true)
    load(initCode)
    run()
  }

  // Blank line between results.
  override def action() = sideEffect(super.action(), printStream.println(""))
}

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

  implicit def showToAmmonite[A](implicit z: Show[A]): pprint.PPrinter[A] =
    pprint.PPrinter[A]((t, c) => vec(z show t).iterator)
}
