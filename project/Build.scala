package fbt

import sbt._, Keys._
import pl.project13.scala.sbt.JmhPlugin

object Build extends sbt.Build with FbtBuild {
  lazy val api = project setup "psp's non-standard api" also spire
  lazy val std = project setup "psp's non-standard standard library" dependsOn api

  def subprojects = List[sbt.Project](api, std)

  def ammoniteArgs   = wordSeq("-encoding utf8 -language:_ -Yno-predef -Yno-imports -Yno-adapted-args")
  def warnArgs       = wordSeq("-deprecation -unchecked -Xfuture -Ywarn-unused -Ywarn-unused-import")
  def noisyArgs      = wordSeq("-Xlint -Ywarn-dead-code -Ywarn-numeric-widen -Ywarn-value-discard")
  def macroDebugArgs = wordSeq("-Ymacro-debug-verbose")
  def optimizeArgs   = wordSeq("-optimise -Yinline-warnings")
  def stdArgs        = ammoniteArgs ++ warnArgs

  lazy val ammoniteTask = Def task {
    val forker    = new Fork("java", Some("psp.ReplMain"))
    val files     = (fullClasspath in Compile in LocalProject("consoleOnly")).value.files filterNot (_.toString contains "scoverage")
    val classpath = files mkString ":"
    val jvmArgs   = Vector(s"-Xbootclasspath/a:$classpath") // boot classpath way faster
    val forkOpts  = ForkOptions(outputStrategy = Some(StdoutOutput), connectInput = true, runJVMOptions = jvmArgs)

    forker(forkOpts, "-usejavacp" +: ammoniteArgs)
  }

  // updateOptions ~=  (_ withCachedResolution true)
  protected def commonSettings(p: Project) = standardSettings ++ Seq(
                  version :=  "0.6.2-SNAPSHOT",
             scalaVersion :=  "2.11.8",
             organization :=  "org.improving",
        coursierVerbosity :=  0,
                 licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
            scalacOptions ++= stdArgs,
         triggeredMessage :=  Watched.clearWhenTriggered,
               incOptions ~=  (_ withNameHashing false) // supposedly we can remove this after sbt commit 65f7958898
  ) ++ p.crossSettings

  lazy val root = (
    project.root.setup.aggregate(projectRefs: _*).dependsOn(classpathDeps: _*) settings (
      console in Compile <<=  console in Compile in consoleOnly,
         console in Test <<=  console in Test in consoleOnly,
            watchSources <++= testing.allSources,
            watchSources <++= consoleOnly.allSources,
            watchSources <++= benchmark.allSources,
                    test <<=  test in testing
    )
    also addCommandAlias("cover", "; clean ; coverage ; test ; coverageReport")
    also addCommandAlias("bench", "benchmark/jmh:run -f1 -t1")
  )

  lazy val consoleOnly = ( project.helper
    dependsOn (classpathDeps: _*)
    dependsOn (testing)
         deps (jsr305, ammonite)
     settings (console in Compile := ammoniteTask.value)
  )
  lazy val testing = project.noArtifacts.setup dependsOn std settings (
                   testOptions +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
                   testOptions +=  Tests.Argument(TestFrameworks.JUnit, "-s"),
     parallelExecution in Test :=  false,
                   logBuffered :=  false,
                          test <<= test in Test,
           libraryDependencies ++= Seq(
              "org.scalacheck" %% "scalacheck"      % "1.12.5",
              "com.novocode"    % "junit-interface" %  "0.11",
              "org.scala-lang"  % "scala-reflect"   % "2.11.8"
            )
  )
  lazy val benchmark = project.noArtifacts.setup dependsOn std enablePlugins JmhPlugin
}
