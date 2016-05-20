package psp
package std

import api._, all._, StdShow._
import java.{ lang => jl }
import java.util.regex.{ Pattern, Matcher }
import jl.Integer.parseInt
import Regex.WS

final class Pstring(val self: String) extends AnyVal with ShowSelf {
  import self.{ toCharArray => chars }

  def *(n: Precise): String                         = Each const self take n show
  def append(that: String): String                  = self + that /** Note to self: don't touch this `+`. */
  def bytes: Array[Byte]                            = self.getBytes
  def capitalize: String                            = zcond(!isEmpty, (self charAt 0).toUpper.to_s append tail.force)
  def charSeq: scSeq[Char]                          = chars.m.seq
  def containsChar(ch: Char): Boolean               = chars.m contains ch
  def format(args: Any*): String                    = stringFormat(self, args: _*)
  def isEmpty: Bool                                 = self === ""
  def lines: View[String]                           = splitChar('\n')
  def lit: Doc                                      = Doc(self)
  def mapChars(pf: Char ?=> Char): String           = chars mapIf pf force
  def mapLines(f: ToSelf[String]): String           = mapSplit('\n')(f)
  def mapSplit(ch: Char)(f: ToSelf[String]): String = splitChar(ch) map f joinWith ch
  def processEscapes: String                        = StringContext processEscapes self
  def r: Regex                                      = Regex(self)
  def removeAll(regex: Regex): String               = regex matcher self replaceAll ""
  def removeFirst(regex: Regex): String             = regex matcher self replaceFirst ""
  def reverseBytes: Array[Byte]                     = bytes.inPlace.reverse
  def reverseChars: String                          = new String(chars.inPlace.reverse)
  def sanitize: String                              = mapChars { case x if x.isControl => '?' }
  def splitChar(ch: Char): View[String]             = splitRegex(Regex quote ch.any_s)
  def splitRegex(r: Regex): View[String]            = r.pattern split self
  def stripMargin(ch: Char): String                 = mapLines(_ stripPrefix WS <> ch.r)
  def stripMargin: String                           = stripMargin('|')
  def stripPrefix(prefix: String): String           = foldPrefix(prefix)(self)(identity)
  def stripSuffix(suffix: String): String           = foldSuffix(suffix)(self)(identity)
  def stripPrefix(r: Regex): String                 = removeAll(r.starts)
  def stripSuffix(r: Regex): String                 = removeAll(r.ends)
  def tail: String                                  = zcond(!isEmpty, self substring 1)
  def to_s: String                                  = self
  def trimLines: String                             = mapLines(_.trim).trim

  // def toDouble: Double = parseDouble(self removeFirst "[dD]$".r)
  // def toFloat: Float   = parseFloat(self removeFirst "[fF]$".r)
  // def toLong: Long     = (self removeFirst "[lL]$".r) |> (s => foldPrefix("0x")(parseLong(s))(parseLong(_, 16)))

  def toBigInt: BigInt = scala.math.BigInt(self)
  def toInt: Int       = foldPrefix("0x")(parseInt(self))(parseInt(_, 16))

  private def foldRemove[A](r: Regex)(none: => A)(some: String => A): A       = removeFirst(r) match { case `self` => none; case s => some(s) }
  private def foldPrefix[A](prefix: String)(none: => A)(some: String => A): A = foldRemove(prefix.r.literal.starts)(none)(some)
  private def foldSuffix[A](suffix: String)(none: => A)(some: String => A): A = foldRemove(suffix.r.literal.ends)(none)(some)
}

class MatcherIterator(m: Matcher) extends scIterator[String] {
  private var nextSeen = false
  def hasNext = {
    if (!nextSeen) nextSeen = m.find
    nextSeen
  }
  def next(): String = cond(
    hasNext,
    sideEffect(m.group, nextSeen = false),
    noSuchElementException("empty.next()")
  )
}

final class Regex(val pattern: Pattern) extends ShowSelf {
  def matcher(input: jCharSequence): Matcher = pattern matcher input

  def iteratorAll(s: String): scIterator[String] = new MatcherIterator(matcher(s))
  def findAll(s: String): Vec[String]            = iteratorAll(s).toVec

  def append(e: String): Regex               = mapRegex(_ + e)
  def capturingGroup: Regex                  = surround("(", ")")
  def characterRange: Regex                  = surround("[", "]")
  def ends: Regex                            = cond(to_s endsWith "$", this, append("$"))
  def flags: Int                             = pattern.flags
  def isMatch(input: jCharSequence): Boolean = matcher(input).matches
  def isMatch[A: Show](x: A): Boolean        = isMatch(x.doc.render)
  def literal: Regex                         = surround("\\Q", "\\E") // Not setFlag(LITERAL) lest further regex additions be misinterpreted
  def mapRegex(f: ToSelf[String]): Regex     = Regex(f(to_s), flags)
  def prepend(s: String): Regex              = mapRegex(s + _)
  def starts: Regex                          = cond(to_s startsWith "^", this, prepend("^"))
  def surround(s: String, e: String): Regex  = mapRegex(s + _ + e)
  def to_s: String                           = s"$pattern"

  def <>(that: Regex): Regex = mapRegex(_ + that.pattern)
  def |(that: Regex): Regex  = mapRegex(_ + "|" + that.pattern)
}

object Regex extends (String => Regex) {
  val WS = apply("\\s*")

  def apply(ch: Char): Regex              = apply(s"[$ch]")
  def quote(s: String): Regex             = apply(Pattern quote s)
  def apply(s: String): Regex             = new Regex(Pattern compile s)
  def apply(s: String, flags: Int): Regex = new Regex(Pattern.compile(s, flags))
}
