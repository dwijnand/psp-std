psp.std - a non-standard library
================================

[![Build Status](https://travis-ci.org/paulp/psp-std.svg?branch=master)](https://travis-ci.org/paulp/psp-std) [![codecov](https://codecov.io/gh/paulp/psp-std/branch/master/graph/badge.svg)](https://codecov.io/gh/paulp/psp-std) [![Join the chat at https://gitter.im/paulp/psp-std](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/paulp/psp-std?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

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
Using libsbt 0.5.7
psp-std repl (ammonite 0.5.6, scala 2.11.7, jvm 1.8.0_74)

psp> 1 to 20 splitAt 10
res0: Split[Int] = Split([ 1, 2, 3, ... ], [ 11, 12, 13, ... ])

psp> 1 to 20 splitAt 10 mapLeft (_.reverse) rejoin
res1: View[Int] = [ 10, 9, 8, ... ]

psp> val xs = 1 to 3 cross vec("a", "bb", "ccc") zipped
xs: ZipView[Int, String] = [ 1 -> a, 1 -> bb, 1 -> ccc, 2 -> a, 2 -> bb, 2 -> ccc, 3 -> a, 3 -> bb, 3 -> ccc ]

psp> val same = xs filter (_ == _.length)
same: ZipView[Int, String] = [ 1 -> a, 2 -> bb, 3 -> ccc ]

psp> println(same.rights mk_s '/')
a/bb/ccc
```

### Requirements

scala 2.11, java 8, sbt 0.13.7+
