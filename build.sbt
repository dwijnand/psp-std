import psp.PspStd._

lazy val root = (
  project.root.setup aggregate std dependsOn std settings (
      coursierVerbosity :=  0,
     console in Compile := (console in Compile in repl).value,
            run in Test := (run in Test in std).evaluated
  )
)

lazy val std = project.setup settings (
                description :=  "psp's non-standard standard library",
        testOptions in Test +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
        testOptions in Test +=  Tests.Argument(TestFrameworks.JUnit, junitArgs: _*),
  parallelExecution in Test :=  false,
        logBuffered in Test :=  false,
         traceLevel in Test :=  30,
        libraryDependencies ++= testDependencies map (_ % "test")
)

lazy val repl = project.noArtifacts.setup dependsOn std settings (
     libraryDependencies += ammonite,
   outputStrategy in run := Some(StdoutOutput),
             fork in run := true,
     connectInput in run := true,
               mainClass := Some("psprepl.Main"),
      console in Compile := (run in Compile).toTask(ammoniteArgs mkString (" ", " ", "")).value
)

lazy val macros = project.noArtifacts dependsOn (std % "compile->compile;test->test") settings (
                 name :=  "psp-macros",
  libraryDependencies +=  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
              version :=  "0.6.2-SNAPSHOT",
         scalaVersion :=  "2.11.8",
   crossScalaVersions :=  Seq(scalaVersion.value, "2.12.0"),
         organization :=  "org.improving",
        scalacOptions ++= wordSeq("-language:experimental.macros -Yno-predef")
)

/*
lazy val benchmark = project.noArtifacts.setup dependsOn std enablePlugins JmhPlugin settings (
  libraryDependencies <++= (scalaBinaryVersion) {
    case "2.11" => Seq(scoverageRuntime, jmhAnnotations)
    case _      => Nil
  }
)

addCommandAlias("bench-min", "benchmark/jmh:run -i 1 -wi 1 -f1 -t1")
addCommandAlias("bench", "benchmark/jmh:run -f1 -t1")
*/

addCommandAlias("cover", "; clean ; coverage ; test ; coverageReport")
