package psp

package object tests extends psp.tests.Explicit with psp.tests.Implicit {
  lazy val isTestDebug = scala.sys.props contains "psp.test.debug"
}
