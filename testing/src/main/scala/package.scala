package psp

import scala.Console.{ println => _, _ }

package object tests extends psp.tests.Explicit with psp.tests.Implicit {
  val PassGreen = GREEN + "\u2713" + RESET // check mark
  val FailRed   = RED + "\u2717" + RESET   // cross mark

  lazy val isTestDebug = scala.sys.props contains "psp.test.debug"
}
