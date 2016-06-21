package psp
package std
package macros

import scala.StringContext
import scala.reflect.macros.blackbox.Context
import all._

/** TODO - mock the ridiculous scala ast with a clean one with the same structure.
 *  Same thing with positions. Then they will be easy to lift and unlift without
 *  struggling with Universes and Importers and Mirrors and all the crazy bullshit
 *  which made it so easy to stop working on scala in the first place.
 *
 *  So for now it's the high-tech approach of calling toString on the tree and
 *  type and passing those, but at least that lets us print something interesting.
 */
sealed trait TypecheckResult {
  def isError: Boolean
  def errorMessage: String
  def tree: String
  def tpe: String
}
final case class Typed(tree: String, tpe: String) extends TypecheckResult {
  def isError = false
  def errorMessage = ""
}
final case class TypeError(errorMessage: String) extends TypecheckResult {
  def isError = true
  def tree: String = "<error>"
  def tpe: String = "<error>"
}
final case class Typechecked(code: String, result: TypecheckResult) {
  def typechecks = !result.isError
  def message    = result.errorMessage
  def tree       = result.tree
  def tpe        = result.tpe
  def to_s       = if (result.isError) s"$code\n$message\n" else s"$tree: $tpe"
}
object Typechecked {
  def error(code: String, message: String): Typechecked           = new Typechecked(code, TypeError(message))
  def typed(code: String, tree: String, tpe: String): Typechecked = new Typechecked(code, Typed(tree, tpe))
}

class Typechecker(val c: Context) {
  import c.universe._

  private def fail(msg: String) = c.abort(c.enclosingPosition, msg)
  private def check(code: String): c.Expr[Typechecked] = c.Expr(
    scala.util.Try(c typecheck (c parse code)) match {
      case scala.util.Success(tree) => q"Typechecked.typed($code, ${tree.toString}, ${tree.tpe.toString})"
      case scala.util.Failure(t)    => q"Typechecked.error($code, ${t.getMessage})"
    }
  )

  def typecheckedLines(exprs: c.Expr[String]): c.Expr[sciVector[Typechecked]] = {
    val code: String = exprs.tree match {
      case Literal(Constant(s: String)) => s.trim
      case _                            => fail("not a literal string")
    }
    def res = (code.lines map (_.trim) filterNot (_.isEmpty) map check).to[sciVector]
    c.Expr(q"$res")
  }

  def typechecked(expr: c.Expr[String]): c.Expr[Typechecked] = check(
    expr.tree match {
      case Literal(Constant(s: String)) => s.trim
      case _                            => fail("not a literal string")
    }
  )
}
