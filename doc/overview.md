naming
======

All projects are built with `-Yno-predef -Yno-imports`. This means even
seemingly built-in types like `Array` and `Int` aren't visible until
we import them. This allows us to deliberately build up a sane namespace from
consciously chosen elements rather than simply dumping in scala.Predef._,
java.lang._, and scala._ as scalac does.

Unfortunately this imposes unnecessary difficulties, because scala provides no
way to perform transparent renaming. The scaffolding of type aliases and val
forwarders to companion objects cannot be hidden or eliminated, nor can the
compiler bugs which come along for the ride. For generic types, those aliases
must redundantly state all the variance markers. Object forwarders have a cost
in bytecode size, indirection, and potentially turn up as null since there is
an actual val being stored in an actual object to assist in the "renaming".

Furthermore, scala provides no way to rename non-value elements at all. For
instance there's no way which can be imported to alias jnf to the
java.nio.file package, because "package is not a value". It is only possible
one file at a time with import renamings like `import java.nio.{ file => jnf }`.
That's why we create aliases for the most common scala types which embed
an abbreviation of their package, such as
```scala
type sciTraversable[+A]     = sci.Traversable[A]
type sciVector[+A]          = sci.Vector[A]
type scmBuilder[-Elem, +To] = scm.Builder[Elem, To]
type scmMap[K, V]           = scm.Map[K, V]
```
So though we can't define sci as an alias for scala.collection.immutable, we
can at least define sciVector to mean Vector.

Scala also offers no way to segment namespaces into smaller units without
inhibiting composition. For instance one would like to express convenience
methods in a trait which could be mixed into a package object wherever it is
convenient to do so. But scala sees them as different entities in each object
where they occur, so this fails to compile:
```scala
trait FinalMethods { final def id[A](x: A): A = x }
object foo extends FinalMethods
object bar extends FinalMethods
class A {
  import foo._, bar._
  id("")
}
// a.scala:6: error: reference to id is ambiguous;
// it is imported twice in the same scope by
// import bar._
// and import foo._
//   id("")
//   ^
// one error found
```
The impact of this is far-reaching. This and other limitations leave us far
short of the reusability which ought to be possible.
