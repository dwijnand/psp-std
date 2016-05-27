package psp
package std

import exp._

package object macros {
  def typechecked(expr: String): Typechecked                  = macro Typechecker.typechecked
  def typecheckedLines(exprs: String): sciVector[Typechecked] = macro Typechecker.typecheckedLines
}
