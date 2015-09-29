scalaVersion := "2.11.6"

name := "corla"

organization := "net.arya"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
	"org.scalaz" %% "scalaz-effect" % "7.1.0",
  "com.github.julien-truffaut"  %%  "monocle-macro" % "1.1.1"
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

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"
addCompilerPlugin("org.spire-math"  %% "kind-projector" % "0.5.2")

