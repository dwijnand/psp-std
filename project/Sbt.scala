package fbt

import sbt._, Keys._

trait FbtBuild {
  self: sbt.Build =>

  def subprojects: List[sbt.Project]
  protected def commonSettings(p: Project): Seq[Setting[_]]

  type InputTaskOf[A] = Def.Initialize[InputTask[A]]
  type SettingOf[A]   = Def.Initialize[A]
  type TaskOf[A]      = Def.Initialize[Task[A]]

  def coursierVerbosity = coursier.CoursierPlugin.autoImport.coursierVerbosity
  def projectRefs       = convertSeq(subprojects): List[ProjectReference]
  def classpathDeps     = convertSeq(subprojects): List[ClasspathDep[ProjectReference]]

  def convertSeq[A, B](xs: List[A])(implicit f: A => B): List[B] = xs map f
  def wordSeq(s: String): Vector[String]                         = scala.Vector(s split "\\s+" filterNot (_ == ""): _*)

  def buildBase     = baseDirectory in ThisBuild
  def jars          = unmanagedJars in Compile
  def testJars      = unmanagedJars in Test

  def spire    = "org.spire-math"           %%     "spire"     %            "0.11.0"
  def jsr305   = "com.google.code.findbugs" %     "jsr305"     %             "3.0.1"
  def ammonite = "com.lihaoyi"              %  "ammonite-repl" % "0.5.7" cross CrossVersion.full

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
  def crossedSourceDirs = List(Test, Compile) map doubleCross

  def standardSettings = crossedSourceDirs ++ Seq(
     javacOptions in compile ++= Seq("-nowarn", "-XDignore.symbol.file"),
    scalacOptions in compile ++= Seq("-language:_", "-Ywarn-unused", "-Ywarn-unused-import"),
          logLevel in update :=  Level.Warn,
     publishArtifact in Test :=  false
  )

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
    def crossSettings = if (id == "root") Nil else Seq(target <<= javaCrossTarget(id))

    def also(m: ModuleID, ms: ModuleID*)     = p settings (m +: ms.toSeq).deps
    def also(s: Setting[_], ss: Setting[_]*) = p settings (s +: ss.toSeq: _*)
    def also(ss: Traversable[Setting[_]])    = p settings (ss.toSeq: _*)
    def deps(ms: ModuleID*)                  = p settings (libraryDependencies ++= ms.toSeq)

    def setup(): Project             = p also commonSettings(p) also (name := s"psp-$id")
    def setup(text: String): Project = setup() also (description := text)
    def hidden(): Project            = p in file(s"./project/$id")
    def helper(): Project            = p.hidden.noArtifacts setup s"helper project $id" dependsOn (classpathDeps: _*)
  }
}
