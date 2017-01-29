scalaVersion := "2.11.8"

name := "corla"

organization := "net.arya"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
	"org.scalaz" %% "scalaz-effect" % "7.1.0",
  "com.github.julien-truffaut"  %%  "monocle-macro" % "1.2.0"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused-import"     // 2.11 only
)

scalacOptions in (Compile, compile) ++= Seq(
  "-Xfatal-warnings"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies ++= Seq("org.specs2" %% "specs2-scalacheck" % "3.6.4" % "test")
scalacOptions in Test ++= Seq("-Yrangepos")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.10.0"
