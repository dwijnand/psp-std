package psp
package std

import api._, all._, StdShow._, StdEq._
import java.{ lang => jl }
import java.util.regex.{ Pattern, Matcher }
import jl.Integer.parseInt // , jl.Long.parseLong, jl.Double.parseDouble, jl.Float.parseFloat

final class SplitCharView(val xs: Vec[String], sep: Char) extends Direct[String] with ShowSelf {
  // private def rebuild(xs: Vec[String]) = new SplitCharView(xs, sep)

  // def collect(pf: String ?=> String): SplitCharView = rebuild(xs collect pf)
  // def filter(p: ToBool[String]): SplitCharView      = rebuild(xs filter p)
  // def grep(r: Regex): SplitCharView                 = rebuild(xs filter r.isMatch)
  // def isEmpty: Bool                                 = xs.isEmpty
  // def map(f: String => String): SplitCharView       = rebuild(xs map f)

  def build(): String                  = xs mk_s sep
  def elemAt(idx: Vdex)                = xs elemAt idx
  def foreach(f: String => Unit): Unit = xs foreach f
  def size: Precise                    = xs.size
  def toVec: Vec[String]               = xs
  def to_s: String                     = build
}

final class Pstring(val self: String) extends AnyVal with ShowSelf {
  import self.{ toCharArray => chars }

  def r: Regex               = Regex(self)
  def to_s: String           = self
  def * (n: Precise): String = Each const self take n join_s
  def isEmpty                = self == ""

  def lines: SplitCharView               = splitView('\n')
  def splitView(ch: Char): SplitCharView = new SplitCharView(splitChar(ch), ch)

  // def bytes: Array[Byte]                      = self.getBytes
  // def charVec: Vec[Char]                      = charSeq.toVec
  // def length: Int                             = self.length
  // def replaceChar(pair: Char -> Char): String = self.replace(pair._1, pair._2)

  def ~(that: String): String                       = this append that
  def append(that: String): String                  = self + that   /** Note to self: don't touch this `+`. */
  def capitalize: String                            = if (isEmpty) "" else (self charAt 0).toUpper.to_s ~ self.tail.force
  def charSeq: scSeq[Char]                          = chars.m.seq
  def containsChar(ch: Char): Boolean               = chars.m contains ch
  def format(args : Any*): String                   = stringFormat(self, args: _*)
  def mapLines(f: ToSelf[String]): String           = mapSplit('\n')(f)
  def mapChars(pf: Char ?=> Char): String           = chars.m mapIf pf force
  def mapSplit(ch: Char)(f: ToSelf[String]): String = splitChar(ch) map f mk_s ch
  def processEscapes: String                        = StringContext processEscapes self
  def removeFirst(regex: Regex): String             = regex matcher self replaceFirst ""
  def removeAll(regex: Regex): String               = regex matcher self replaceAll ""
  def reverseChars: String                          = new String(chars.inPlace.reverse)
  def sanitize: String                              = mapChars { case x if x.isControl => '?' }
  def splitChar(ch: Char): Vec[String]              = splitRegex(Regex quote ch.any_s)
  def splitRegex(r: Regex): Vec[String]             = r.pattern split self toVec
  def stripMargin(ch: Char): String                 = mapLines(_ removeFirst s"""^${bs}s*[$ch]""".r)
  def stripMargin: String                           = stripMargin('|')
  def stripPrefix(prefix: String): String           = foldPrefix(prefix)(self)(identity)
  def stripSuffix(suffix: String): String           = foldSuffix(suffix)(self)(identity)
  def trimLines: String                             = mapLines(_.trim).trim

  // def toDecimal: BigDecimal = scala.math.BigDecimal(self)
  // def toDouble: Double      = parseDouble(self removeFirst "[dD]$".r)
  // def toFloat: Float        = parseFloat(self removeFirst "[fF]$".r)
  // def toLong: Long          = (self removeFirst "[lL]$".r) |> (s => foldPrefix("0x")(parseLong(s))(parseLong(_, 16)))

  def toSafeLong: SafeLong = SafeLong(self removeAll """\s+""".r toBigInt) // spire allows spaces in their literal syntax
  def toBigInt: BigInt     = scala.math.BigInt(self)
  def toInt: Int           = foldPrefix("0x")(parseInt(self))(parseInt(_, 16))

  private def bs = '\\'
  private def foldRemove[A](r: Regex)(none: => A)(some: String => A): A       = removeFirst(r) match { case `self` => none ; case s => some(s) }
  private def foldPrefix[A](prefix: String)(none: => A)(some: String => A): A = foldRemove(prefix.r.literal.starts)(none)(some)
  private def foldSuffix[A](suffix: String)(none: => A)(some: String => A): A = foldRemove(suffix.r.literal.ends)(none)(some)
}

final class Regex(val pattern: Pattern) extends AnyVal with ShowSelf {
  def matcher(input: jCharSequence): Matcher = pattern matcher input

  def append(e: String): Regex               = mapRegex(_ + e)
  def capturingGroup: Regex                  = surround("(", ")")
  // def characterClass: Regex                  = surround("[", "]")
  def ends: Regex                            = append("$")
  def flags: Int                             = pattern.flags
  def isMatch(input: jCharSequence): Boolean = matcher(input).matches
  def isMatch[A: Show](x: A): Boolean        = isMatch(x.render)
  def literal: Regex                         = surround("\\Q", "\\E") // Not setFlag(LITERAL) lest further regex additions be misinterpreted
  def mapRegex(f: ToSelf[String]): Regex     = Regex(f(to_s), flags)
  def starts: Regex                          = mapRegex("^" + _)
  def surround(s: String, e: String): Regex  = mapRegex(s + _ + e)
  def to_s: String                           = s"$pattern"

  def |(that: Regex): Regex = mapRegex(_ + "|" + that.pattern)
  // def ~(that: Regex): Regex = mapRegex(_ + that.pattern)
}

object Regex extends (String => Regex) {
  // def alt(r1: Regex, r2: Regex): Regex = r1 | r2

  def quote(s: String): Regex             = apply(Pattern quote s)
  def apply(s: String): Regex             = new Regex(Pattern compile s)
  def apply(s: String, flags: Int): Regex = new Regex(Pattern.compile(s, flags))
}
