package scala.tools.nsc.interpreter {
  trait ReplPrinter[-A] extends scala.Any {
    def print(x: A, maxElements: scala.Int): java.lang.String
  }
}

package psp {
  package std {
    import all._

    import scala.tools.nsc.interpreter.ReplPrinter

    class PspReplPrinter[A](implicit z: Show[A]) extends ReplPrinter[A] {
      def print(x: A, maxElements: Int): String = {
        val s = z show x
        val nl = if (s contains "\n") "\n" else ""
        nl + s + "\n"
      }
    }
  }
}

