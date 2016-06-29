package psp

import sbt._, Keys._

object PspStd {
  type SettingOf[A]   = Def.Initialize[A]
  type TaskOf[A]      = Def.Initialize[Task[A]]
  type InputTaskOf[A] = Def.Initialize[InputTask[A]]

  def junitArgs: Seq[String]    = sys.env.getOrElse("ARGS", "-a -s").split("\\s+").toSeq
  def baseArgs: Seq[String]     = wordSeq("-language:_ -Yno-predef -Yno-adapted-args")
  def ammoniteArgs: Seq[String] = baseArgs ++ wordSeq("-Yno-imports")
  def compileArgs: Seq[String]  = ammoniteArgs ++ wordSeq("-Ywarn-unused -Ywarn-unused-import")
  def compileArgsBoth           = inBoth(config => Seq(scalacOptions in compile in config ++= compileArgs))

  def ammonite         = "com.lihaoyi"              %       "ammonite-repl"       % "0.6.2" cross CrossVersion.full
  def jmhAnnotations   = "org.openjdk.jmh"          %  "jmh-generator-annprocess" %             "1.12"
  def jsr305           = "com.google.code.findbugs" %           "jsr305"          %             "3.0.1"
  def scoverageRuntime = "org.scoverage"            %% "scalac-scoverage-runtime" %             "1.1.1"

  def testDependencies = Seq(
    "org.scalacheck" %% "scalacheck"      % "1.13.1",
    "com.novocode"    % "junit-interface" %  "0.11"
  )

  // To sync with Maven central, you need to supply the following information:
  def pspPomExtra = (
    <url>https://github.com/paulp/psp-std</url>
    <scm>
      <connection>scm:git:github.com/paulp/psp-std</connection>
      <developerConnection>scm:git:git@github.com:paulp/psp-std</developerConnection>
      <url>https://github.com/paulp/psp-std</url>
    </scm>
    <developers>
      <developer>
        <id>paulp</id>
        <name>Paul Phillips</name>
        <url>https://github.com/paulp/</url>
      </developer>
    </developers>
  )

  def commonSettings = compileArgsBoth ++ inBoth(doubleCross) ++ Seq(
                   pomExtra :=  pspPomExtra,
    javacOptions in compile ++= Seq("-nowarn", "-XDignore.symbol.file"),
         logLevel in update :=  Level.Warn,
    publishArtifact in Test :=  false,
        libraryDependencies +=  jmhAnnotations,
                    version :=  "0.6.2-SNAPSHOT",
               scalaVersion :=  "2.11.8",
               organization :=  "org.improving",
                   licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
           triggeredMessage :=  Watched.clearWhenTriggered,
                 incOptions ~=  (_ withNameHashing false) // supposedly we can remove this after sbt commit 65f7958898
  )

  def wordSeq(s: String): Vector[String] = scala.Vector(s split "\\s+" filterNot (_ == ""): _*)

  def inBoth[A](f: Configuration => Seq[A]): Seq[A] = List(Test, Compile) flatMap f

  def buildBase                   = baseDirectory in ThisBuild
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

    def root: Project = noArtifacts in file(".")
    def noArtifacts: Project = also(
                publish := (()),
           publishLocal := (()),
         Keys.`package` := file(""),
             packageBin := file(""),
      packagedArtifacts := Map()
    )
    def allSources = Def task (sources in Test in p).value ++ (sources in Compile in p).value

    def also(m: ModuleID, ms: ModuleID*): Project     = also(libraryDependencies ++= m +: ms)
    def deps(ms: ModuleID*): Project                  = also(libraryDependencies ++= ms.toSeq)
    def also(s: Setting[_], ss: Setting[_]*): Project = also(s +: ss.toSeq)
    def also(ss: Seq[Setting[_]]): Project            = p settings (ss: _*)

    def setup(): Project = p also commonSettings also (
        name :=  s"psp-$id",
      target <<= javaCrossTarget(id)
    )
  }
}
