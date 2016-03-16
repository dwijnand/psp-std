package fbt

import sbt._, Keys._

trait Ammonite {
  def ammoniteArgs: Seq[String]

  lazy val ammoniteTask = Def task {
    val forker    = new Fork("java", Some("psp.ReplMain"))
    val files     = (fullClasspath in Compile in LocalProject("consoleOnly")).value.files filterNot (_.toString contains "scoverage")
    val classpath = files mkString ":"
    val jvmArgs   = Vector(s"-Xbootclasspath/a:$classpath") // boot classpath way faster
    val forkOpts  = ForkOptions(outputStrategy = Some(StdoutOutput), connectInput = true, runJVMOptions = jvmArgs)

    forker(forkOpts, "-usejavacp" +: ammoniteArgs)
  }
}
