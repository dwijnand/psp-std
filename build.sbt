val root        = fbt.Build.root
val std         = fbt.Build.std
val consoleOnly = fbt.Build.consoleOnly
val testing     = fbt.Build.testing
val benchmark   = fbt.Build.benchmark

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
