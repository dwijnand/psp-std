import psp.PspStd._

organization in ThisBuild := "org.improving"
     version in ThisBuild := "0.6.3-SNAPSHOT"
scalaVersion in ThisBuild := "2.12.1"
    licenses in ThisBuild := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

lazy val root = (
  project.root.setup aggregate std dependsOn std settings (
    console in Compile := (console in Compile in std).value,
           run in Test := (run in Test in std).evaluated
  )
)

lazy val std = project.setup settings (
                description :=  "psp's non-standard standard library",
        testOptions in Test +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
        testOptions in Test +=  Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),
  parallelExecution in Test :=  false,
        logBuffered in Test :=  false,
         traceLevel in Test :=  30,
                   pomExtra :=  pspPomExtra,
        libraryDependencies ++= Seq(
          "org.scalacheck" %% "scalacheck"      % "1.13.5" % Test,
          "com.novocode"    % "junit-interface" %  "0.11" % Test
        )

)

lazy val macros = project.noArtifacts dependsOn (std % "compile->compile;test->test") settings (
                 name := "psp-macros",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
        scalacOptions += "-language:experimental.macros"
)

lazy val benchmark = project.noArtifacts.setup dependsOn std enablePlugins JmhPlugin

addCommandAlias("bench-min", "benchmark/jmh:run -i 1 -wi 1 -f1 -t1")
addCommandAlias("bench", "benchmark/jmh:run -f1 -t1")
addCommandAlias("cover", "; clean ; coverage ; test ; coverageReport")
