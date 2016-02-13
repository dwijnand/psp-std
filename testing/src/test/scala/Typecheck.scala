package psp
package tests

import psp._, std._, all._, api._, macros._
import Expressions._

class Typecheck extends ScalacheckBundle {
  def bundle = "Verifying Expected Failures"

  /** These are here so we do not accidentally rename them, making the shadowing below
   *  useless.
   */
  private val _ = {
    opsWrapString _
    viewPspString _
  }

  // We don't want to protect scala library from itself so let's unmask augmentString etc.
  def checkScala() = {

    // These two definitions are here to shadow implicits
    val opsWrapString = null
    val viewPspString = null
    identity(opsWrapString) ; identity(viewPspString) // suppress "never used" warnings
    // This import is actually used in the test below
    import scala.Predef._
    divide("scala-library", typecheckedLines(scalaLibraryCode), expectedTypecheck = 24)
  }

  /** We'll say a line which begins with the shown comment is expected to type check.
   *  Will make this more robust. For now this makes it easy to put the expectation of
   *  success or failure next to the code in question.
   */
  def divide(what: String, xs: Vec[Typechecked]): NamedProp = divide(what, xs, xs count (_.code startsWith "/* ok */"))

  def divide(what: String, xs: Vec[Typechecked], expectedTypecheck: Int): NamedProp = {
    val (good, bad) = xs.toScalaVector partition (_.typechecks)
    NamedProp(
      s"$expectedTypecheck/${xs.size} expressions from $what should typecheck",
      (Prop(expectedTypecheck == good.size) :|
        ("good:\n  " + (good mkString "\n  ") + "\n\nbad:\n  " + (bad mkString "\n  ") + "\n")
      )
    )
  }

  def props = vec[NamedProp](
    divide("psp-show", typecheckedLines(pspShowCode)),
    divide("psp-by-equals", typecheckedLines(pspByEquals), expectedTypecheck = 12),
    divide("psp-by-ref", typecheckedLines(pspByRef), expectedTypecheck = 0),
    divide("psp-straight", typecheckedLines(scalaLibraryCode), expectedTypecheck = 14),
    checkScala()
  )
}
