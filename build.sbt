import psp.PspStd._

lazy val std = project.setup settings (
  description :=  "psp's non-standard standard library"
)

lazy val root = (
  project.root.setup aggregate std dependsOn (std, testing) settings (
                       coursierVerbosity  :=  0,
                     libraryDependencies  +=  ammonite,
                      console in Compile <<=  ammoniteTask,
   unmanagedSourceDirectories in Compile  +=  buildBase.value / "project" / "repl",
                            watchSources <++= testing.allSources,
                            watchSources <++= benchmark.allSources,
                                    test <<=  test in testing
  )
)

lazy val testing = project.noArtifacts.setup dependsOn std settings (
                 testOptions +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
                 testOptions +=  Tests.Argument(TestFrameworks.JUnit, "-s"),
   parallelExecution in Test :=  false,
                 logBuffered :=  false,
                        test <<= test in Test,
         libraryDependencies ++= testDependencies
)
lazy val benchmark = project.noArtifacts.setup dependsOn std enablePlugins JmhPlugin settings (
  libraryDependencies ++= ( if (scalaBinaryVersion.value == "2.11") Seq(scoverageRuntime) else Seq() )
)


addCommandAlias("bench-min", "benchmark/jmh:run -i 1 -wi 1 -f1 -t1")
addCommandAlias("cover", "; clean ; coverage ; bench-min ; test ; coverageReport")
addCommandAlias("bench", "benchmark/jmh:run -f1 -t1")
