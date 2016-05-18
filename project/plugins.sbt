  scalacOptions += "-deprecation"
ivyLoggingLevel := UpdateLogging.Quiet

addSbtPlugin("org.scoverage"       % "sbt-scoverage"           %   "1.3.5")
addSbtPlugin("com.jsuereth"        % "sbt-pgp"                 %   "1.0.0")
addSbtPlugin("org.xerial.sbt"      % "sbt-sonatype"            %    "1.1")
addSbtPlugin("io.get-coursier"     % "sbt-coursier"            % "1.0.0-M12")
addSbtPlugin("com.updateimpact"    % "updateimpact-sbt-plugin" %   "2.1.1")
addSbtPlugin("pl.project13.scala"  % "sbt-jmh"                 %   "0.2.6")
addSbtPlugin("com.geirsson"       %% "sbt-scalafmt"            %   "0.2.5")
