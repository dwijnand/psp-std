package psp

import sbt._, Keys._

object PspStd {
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

  def wordSeq(s: String): Vector[String] = scala.Vector(s split "\\s+" filterNot (_ == ""): _*)

  def inBoth[A](f: Configuration => Seq[A]): Seq[A] = List(Test, Compile) flatMap f

  def buildBase                   = baseDirectory in ThisBuild
  def javaSpecVersion: String     = sys.props("java.specification.version")
  def javaCrossTarget(id: String) = buildBase(_ / "target" / id / s"java_$javaSpecVersion")

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
  implicit class ProjectOps(val p: Project) {
    import p.id

    def typelevelArgs = wordSeq("-Ypartial-unification -Yliteral-types")
    def typelevel     = also(scalaOrganization := "org.typelevel", scalacOptions += "-Ypartial-unification")

    def root: Project = noArtifacts in file(".")
    def noArtifacts: Project = also(
                publish := (()),
           publishLocal := (()),
         Keys.`package` := file(""),
             packageBin := file(""),
      packagedArtifacts := Map()
    )

    def also(m: ModuleID, ms: ModuleID*): Project     = also(libraryDependencies ++= m +: ms)
    def deps(ms: ModuleID*): Project                  = also(libraryDependencies ++= ms.toSeq)
    def also(s: Setting[_], ss: Setting[_]*): Project = also(s +: ss.toSeq)
    def also(ss: Seq[Setting[_]]): Project            = p settings (ss: _*)

    def setup(): Project = p also inBoth(doubleCross) also (
      scalacOptions in compile ++= wordSeq("-language:_ -Yno-adapted-args -Ywarn-unused -Ywarn-unused-import"),
              triggeredMessage :=  Watched.clearWhenTriggered,
                          name :=  s"psp-$id",
                        target :=  javaCrossTarget(id).value
    )
  }
}
