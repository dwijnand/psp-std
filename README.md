psp.std - a non-standard library
================================

[![Build Status](https://travis-ci.org/paulp/psp-std.svg?branch=master)](https://travis-ci.org/paulp/psp-std) [![codecov](https://codecov.io/gh/paulp/psp-std/branch/master/graph/badge.svg?bloop)](https://codecov.io/gh/paulp/psp-std) [![Join the chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/paulp/psp-std)

### Background

The scala standard library is deficient in many ways. This library is an attempt to rebuild it with some attention given to consistency, performance, and correctness. See [views](doc/views.md) for some details. See [overview](doc/overview.md) for the project layout.

### Usage

Suggested contents for a basic `build.sbt` follows. Note that the console transcript as seen requires more project code than this.

```scala
              scalaVersion :=  "2.11.8"
                 resolvers +=  Opts.resolver.sonatypeReleases
             scalacOptions ++= Seq("-language:_", "-Yno-predef")
initialCommands in console :=  "import psp.std._, all._, StdShow._"
       libraryDependencies +=  "org.improving" %% "psp-std" % "0.6.1"
```

Then `sbt console` and you can look around.
```scala
scala> val xs = 1 to 20 splitAfter 10.size
xs: psp.std.RView[psp.std.all.Int,psp.std.all.ClosedRange[psp.std.all.Int]]#Split = [ 1, 2, 3, ... ] / [ 11, 12, 13, ... ]

scala> xs mapLeft (_ dropRight 8) join
res0: psp.std.RView[Int,psp.std.Consecutive.Closed[Int]] = [ 1, 2, 11, ... ]

scala> xs.zip filterRight (_ % 3 === 0)
res1: psp.std.Zip[Int,Int] = [ 2 -> 12, 5 -> 15, 8 -> 18 ]

scala> val ys = zipCross(1 to 3, view("a", "bb"))
ys: psp.std.Zip[psp.std.all.Int,String] = [ 1 -> a, 1 -> bb, 2 -> a, 2 -> bb, 3 -> a, 3 -> bb ]

scala> val zs = ys eqBy (x => x, _.length)
zs: psp.std.Zip[psp.std.all.Int,String] = [ 1 -> a, 2 -> bb ]

scala> zs.rights joinWith '/'
res2: psp.std.all.String = a/bb
```

### Requirements

scala 2.11/2.12, java 8, sbt 0.13.7+
