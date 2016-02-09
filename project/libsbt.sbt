addSbtPlugin("org.improving" % "psp-libsbt" % sys.props.getOrElse("libsbt.version", "0.6.0"))

resolvers += Resolver.url("paulp/sbt-plugins", url("https://dl.bintray.com/paulp/sbt-plugins"))(Resolver.ivyStylePatterns)

resolvers += Resolver.jcenterRepo
