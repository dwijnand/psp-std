val root      = pspstd.PspStd.root
val std       = pspstd.PspStd.std
val testing   = pspstd.PspStd.testing
val benchmark = pspstd.PspStd.benchmark

// To sync with Maven central, you need to supply the following information:
pomExtra in Global := (
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
