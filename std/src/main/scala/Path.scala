package psp
package std

import all._
// import api._, all._
import java.nio.{ file => jnf }
import java.nio.file.{ attribute => jnfa }
import jnf._, jnfa.BasicFileAttributes

/** Memoization for the results of Path => A functions, where
 *  it's assumed the path translates into an actual file. The file
 *  modification time determines whether the cached value needs
 *  to be recalculated.
 */
// class PathCache[A](f: jPath => A) extends (jPath => A) {
//   private[this] val timestamps = bufferMap[jPath, FileTime]()
//   private[this] val content    = scmMap[jPath, A]()
//   private def timestampOk(path: jPath) = path.lastModified == timestamps(path)
//   private def updateCache(path: jPath): A = {
//     timestamps(path) = path.lastModified
//     doto(f(path))(content(path) = _)
//   }
//   def clear(): Unit = {
//     timestamps.clear()
//     content.clear()
//   }
//   def apply(path: jPath): A = content get path match {
//     case Some(c) if timestampOk(path) => c
//     case _                            => updateCache(path)
//   }
// }

/** PathVisitor.
 */
// trait PathVisitor extends FileVisitor[Path] {
//   def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult
//   def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult
//   def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult
//   def visitFileFailed(file: Path, exc: IOException): FileVisitResult
// }

/** Standard memoizing objects for the given types.
 */
// object PathBytes extends PathCache[Array[Byte]](Files readAllBytes _)
// object PathChars extends PathCache[Array[Char]](path => utf8(PathBytes(path)).chars)
// object PathLines extends PathCache[View[String]](Files readAllLines _ m)
// object PathSlurp extends PathCache[String](path => utf8(PathBytes(path)).to_s)

// object PathVisitor {
//   val Continue     = FileVisitResult.CONTINUE
//   val SkipSiblings = FileVisitResult.SKIP_SIBLINGS
//   val SkipSubtree  = FileVisitResult.SKIP_SUBTREE
//   val Terminate    = FileVisitResult.TERMINATE

//   class ForeachPath(fn: (Path, BasicFileAttributes) => FileVisitResult) extends SimpleFileVisitor[Path] with PathVisitor {
//     override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = fn(file, attrs)
//     override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = exc match {
//       case _: IOException                      => Continue
//       case _                                   => throw exc
//     }
//   }

//   def apply(f: (Path, BasicFileAttributes) => FileVisitResult): PathVisitor = new ForeachPath(f)
//   def simple(f: Path => Unit): PathVisitor                                   = apply((file, _) => sideEffect(Continue, f(file)))
// }

package ops {
  class PathOps(path: jPath) {
    // def /(p: String): jPath = path resolve p
    // def /(p: jPath): jPath  = path resolve p

    def attrs[A <: BasicFileAttributes : CTag](opts: LinkOption*): A = Files.readAttributes(path, classOf[A], opts: _*)
    // def lastModified: FileTime                                       = optMap(attrs[BasicFileAttributes]())(_.lastModifiedTime).zget
    // def walk(visitor: PathVisitor): Unit                             = Files.walkFileTree(path, visitor)
    // def foreach(f: Path => Unit): Unit                               = walk(PathVisitor simple f)

    // def parent(): jPath  = Try(path.getParent) | path
    // def follow(): jPath  = Try(readSymbolicLink) | path
    // def isSymbolicLink   = Files isSymbolicLink path
    // def readSymbolicLink = Files readSymbolicLink path
    // def segments         = 0 until path.getNameCount map (path getName _)

    // def followFrom(cwd: jPath): jPath = follow() |> (p => if (p.isAbsolute) p else p.segments.foldl(cwd)(_ / _))
    // def followResolve(): jPath        = followFrom(parent)

    // def readAllBytes(): Array[Byte]                = jnf.Files.readAllBytes(path)
    // def readAllLines(cs: jCharset): Direct[String] = jnf.Files.readAllLines(path, cs).toVec
    // def readAllLines(): Direct[String]             = readAllLines(defaultCharset)
  }
}
