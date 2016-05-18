package pspstd

import sbt._, Keys._
import pl.project13.scala.sbt.JmhPlugin
import coursier.CoursierPlugin.autoImport.coursierVerbosity

object PspStd extends SbtPlumbing {
  lazy val std = project setup "psp's non-standard standard library"

  def baseArgs: Seq[String]     = wordSeq("-language:_ -Yno-predef -Yno-adapted-args")
  def ammoniteArgs: Seq[String] = baseArgs ++ wordSeq("-Yno-imports")
  def compileArgs: Seq[String]  = ammoniteArgs ++ wordSeq("-Ywarn-unused -Ywarn-unused-import")
  def compileArgsBoth           = inBoth(config => Seq(scalacOptions in compile in config ++= compileArgs))

  def subprojects = List(std)

  def commonSettings = compileArgsBoth ++ Seq(
     javacOptions in compile ++= Seq("-nowarn", "-XDignore.symbol.file"),
          logLevel in update :=  Level.Warn,
     publishArtifact in Test :=  false,
         libraryDependencies +=  jmhAnnotations,
                     version :=  "0.6.2-SNAPSHOT",
                scalaVersion :=  "2.11.8",
                organization :=  "org.improving",
           coursierVerbosity :=  0,
                    licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
            triggeredMessage :=  Watched.clearWhenTriggered,
                  incOptions ~=  (_ withNameHashing false) // supposedly we can remove this after sbt commit 65f7958898
  )

  lazy val ammoniteTask: TaskOf[Unit] = Def task {
    val forker    = new Fork("java", Some("psp.ReplMain"))
    val files     = (fullClasspath in Compile in LocalProject("root")).value.files filterNot (_.toString contains "scoverage")
    val classpath = files mkString ":"
    val jvmArgs   = Vector(s"-Xbootclasspath/a:$classpath") // boot classpath way faster
    val forkOpts  = ForkOptions(outputStrategy = Some(StdoutOutput), connectInput = true, runJVMOptions = jvmArgs)

    forker(forkOpts, "-usejavacp" +: ammoniteArgs)
    ()
  }

  lazy val root = (
    project.root.setup.aggregate(projectRefs: _*).dependsOn(classpathDeps: _*) settings (
                       libraryDependencies  +=  ammonite,
                        console in Compile <<=  ammoniteTask,
     unmanagedSourceDirectories in Compile  +=  buildBase.value / "project" / "repl",
                              watchSources <++= testing.allSources,
                              watchSources <++= benchmark.allSources,
                                      test <<=  test in testing
    )
    also addCommandAlias("bench-min", "benchmark/jmh:run -i 1 -wi 1 -f1 -t1")
    also addCommandAlias("cover", "; clean ; coverage ; bench-min ; test ; coverageReport")
    also addCommandAlias("bench", "benchmark/jmh:run -f1 -t1")
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
  lazy val benchmark = project.noArtifacts.setup dependsOn std enablePlugins JmhPlugin settings (
    libraryDependencies ++= ( if (scalaBinaryVersion.value == "2.11") Seq(scoverageRuntime) else Seq() )
  )
}

abstract class SbtPlumbing {
  def subprojects: List[sbt.Project]
  def commonSettings: Seq[Setting[_]]

  def projectRefs   = convertSeq[ProjectReference](subprojects)
  def classpathDeps = convertSeq[ClasspathDep[ProjectReference]](subprojects)

  type InputTaskOf[A] = Def.Initialize[InputTask[A]]
  type SettingOf[A]   = Def.Initialize[A]
  type TaskOf[A]      = Def.Initialize[Task[A]]

  def ammonite         = "com.lihaoyi"              %       "ammonite-repl"       % "0.5.7" cross CrossVersion.full
  def jmhAnnotations   = "org.openjdk.jmh"          %  "jmh-generator-annprocess" %             "1.12"
  def jsr305           = "com.google.code.findbugs" %           "jsr305"          %             "3.0.1"
  def scoverageRuntime = "org.scoverage"            %% "scalac-scoverage-runtime" %             "1.1.1"

  def convertSeq[B] = new {
    def apply[A](xs: List[A])(implicit f: A => B): List[B] = xs map f
  }

  def wordSeq(s: String): Vector[String] = scala.Vector(s split "\\s+" filterNot (_ == ""): _*)

  def inBoth[A](f: Configuration => Seq[A]): Seq[A] = List(Test, Compile) flatMap f
  def buildBase                   = baseDirectory in ThisBuild
  def toolsJar                    = Seq("..", "lib", "tools.jar").foldLeft(file(sys.props("java.home")))(new java.io.File(_, _))
  def javaSpecVersion: String     = sys.props("java.specification.version")
  def javaCrossTarget(id: String) = buildBase mapValue (_ / "target" / id / s"java_$javaSpecVersion")

  /** Watch out Jonesy! It's the ol' double-cross!
   *  Why, you...
   *
   *  Given a path like src/main/scala we want that to explode into something like the
   *  following, assuming we're currently building with java 1.7 and scala 2.10.
   *
   *    src/main/scala
   *    src/main/scala_2.10
   *    src/main_1.7/scala
   *    src/main_1.7/scala_2.10
   *
   *  Similarly for main/test, 2.10/2.11, 1.7/1.8.
   */
  def doubleCross(config: Configuration) = {
    unmanagedSourceDirectories in config ++= {
      val jappend = Seq("", "_" + javaSpecVersion)
      val sappend = Seq("", "_" + scalaBinaryVersion.value)
      val basis   = (sourceDirectory in config).value
      val parent  = basis.getParentFile
      val name    = basis.getName
      for (j <- jappend ; s <- sappend) yield parent / s"$name$j" / s"scala$s"
    }
  }

  implicit class SeqModuleIDOps(val ids: Seq[ModuleID]) {
    def deps(): Setting[_]            = libraryDependencies ++= ids
    def intransitive(): Seq[ModuleID] = ids map (_.intransitive())
    def inTest(): Seq[ModuleID]       = ids map (_ % "test")
    def inCompile(): Seq[ModuleID]    = ids map (_ % "compile")
  }
  implicit class SettingOfOps[A](val s: SettingOf[A]) {
    def task: TaskOf[A]                      = Def task s.value
    def mapValue[B](f: A => B): SettingOf[B] = Def setting f(s.value)
  }
  implicit class SettingKeyOps[A](val key: SettingKey[A]) {
    // For a project as obsessed with "map" as is sbt, how did they manage to
    // fuck up calling "map" on a setting? It is implicitly converted to a Task
    // or something, and you don't get a setting back.
    def mapValue[B](f: A => B): SettingOf[B] = Def setting f(key.value)
  }
  implicit class KeyPairOps[A, B](val pair: (SettingKey[A], SettingKey[B])) {
    def mapValue[C](f: (A, B) => C): SettingOf[C] = Def setting f(pair._1.value, pair._2.value)
  }
  implicit class ProjectOps(val p: Project) {
    import p.id

    def root: Project        = p.noArtifacts in file(".")
    def noArtifacts: Project = also(publish := (()), publishLocal := (()), Keys.`package` := file(""), packageBin := file(""), packagedArtifacts := Map())

    def allSources    = Def task (sources in Test in p).value ++ (sources in Compile in p).value
    def usesCompiler  = p settings (libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value)
    def usesReflect   = p settings (libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)
    def crossSettings = inBoth(doubleCross) ++ ( if (id == "root") Nil else Seq(target <<= javaCrossTarget(id)) )

    def also(m: ModuleID, ms: ModuleID*)     = p settings (m +: ms.toSeq).deps
    def also(s: Setting[_], ss: Setting[_]*) = p settings (s +: ss.toSeq: _*)
    def also(ss: Traversable[Setting[_]])    = p settings (ss.toSeq: _*)
    def deps(ms: ModuleID*)                  = p settings (libraryDependencies ++= ms.toSeq)

    def setup(): Project             = p also (crossSettings ++ commonSettings) also (name := s"psp-$id")
    def setup(text: String): Project = setup() also (description := text)
    def hidden(): Project            = p in file(s"./project/$id")
    def helper(): Project            = p.hidden.noArtifacts setup s"helper project $id" dependsOn (classpathDeps: _*)
  }
}
