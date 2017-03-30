import psp.PspStd._

lazy val root = (
  project.root.setup aggregate std dependsOn std settings (
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

lazy val macros = project.noArtifacts dependsOn (std % "compile->compile;test->test") settings universalSettings settings (
                 name := "psp-macros",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        scalacOptions += "-language:experimental.macros"
)

lazy val benchmark = project.noArtifacts.setup dependsOn std enablePlugins JmhPlugin

/*

*/

addCommandAlias("bench-min", "benchmark/jmh:run -i 1 -wi 1 -f1 -t1")
addCommandAlias("bench", "benchmark/jmh:run -f1 -t1")
addCommandAlias("cover", "; clean ; coverage ; test ; coverageReport")
