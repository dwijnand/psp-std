psp.std - a non-standard library
================================

[![Build Status](https://travis-ci.org/paulp/psp-std.svg?branch=master)](https://travis-ci.org/paulp/psp-std) [![codecov](https://codecov.io/gh/paulp/psp-std/branch/master/graph/badge.svg?bloop)](https://codecov.io/gh/paulp/psp-std) [![Join the chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/paulp/psp-std)

### Background

The scala standard library is deficient in many ways. This library is an attempt to rebuild it with some attention given to consistency, performance, and correctness. See [views](doc/views.md) for some details. See [overview](doc/overview.md) for the project layout.

### Usage

Suggested contents for a basic `build.sbt` follows. Note that the console transcript as seen requires more project code than this. The standard scala repl can't handle -Yno-imports, so psp-std derives a new console task from ammonite.

```scala
              scalaVersion :=  "2.11.8"
                 resolvers +=  Opts.resolver.sonatypeReleases
             scalacOptions ++= Seq("-language:_", "-Yno-predef")
initialCommands in console :=  "import psp._, std._, all._, api._, StdEq._, StdShow._"
       libraryDependencies +=  "org.improving" %% "psp-std" % "0.6.1"
```

Then `sbt console` and you can look around.
```scala
% sbt console
psp-std repl (ammonite 0.5.7, scala 2.11.8, jvm 1.8.0_92)

psp> val xs = 1 to 20 splitAt 10
xs: Split[Int] = Split([ 1, 2, 3, ... ], [ 11, 12, 13, ... ])

psp> xs mapLeft (_ dropRight 8) rejoin
res0: View[Int] = [ 1, 2, 11, ... ]

psp> xs.zipped filterRight (_ % 3 == 0)
res1: ZipView[Int, Int] = [ 2 -> 12, 5 -> 15, 8 -> 18 ]

psp> val ys = 1 to 3 cross vec("a", "bb", "ccc") zipped
ys: ZipView[Int, String] = [ 1 -> a, 1 -> bb, 1 -> ccc, 2 -> a, 2 -> bb, 2 -> ccc, 3 -> a, 3 -> bb, 3 -> ccc ]

psp> val same = ys filter (_ === _.length)
same: ZipView[Int, String] = [ 1 -> a, 2 -> bb, 3 -> ccc ]

psp> same.rights mk_s '/'
res2: String = a/bb/ccc
```

### Requirements

scala 2.11, java 8, sbt 0.13.7+
