package psp
package std

trait ZeroOne[+A] {
  def zero: A
  def one: A
}
trait ZeroOne0 {
  self: ZeroOne.type =>

  implicit val zeroSize: ZeroOne[Size] = make[Size](Size(0), Size(1))
}
object ZeroOne {
  def make[A](z: A, o: A): ZeroOne[A] = new ZeroOne[A] {
    def zero: A = z
    def one: A  = o
  }

  implicit val zeroIndex: ZeroOne[Index]     = make(Index(0), Index(1))
  implicit val zeroPrecise: ZeroOne[Precise] = make(Size(0), Size(1))
}
