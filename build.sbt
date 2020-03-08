organization := "io.github.montokapro"
name := "goblins"
version := "0.0.1-SNAPSHOT"
crossScalaVersions := Seq("2.12.10", "2.13.1")

resolvers += Resolver.sonatypeRepo("snapshots")

val CatsVersion = "2.1.1"
val KindProjectorVersion = "0.11.0"
val LogbackVersion = "1.2.3"
val ScalaCheckVersion = "1.14.3"
val ScalaTestVersion = "3.1.1"
val ScalaTestPlusVersion = "3.1.1.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.typelevel" %% "cats-free" % CatsVersion,
  "ch.qos.logback" % "logback-classic" % LogbackVersion,
  "org.scalacheck" %% "scalacheck" % ScalaCheckVersion % Test,
  "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % ScalaTestPlusVersion % Test,
)

addCompilerPlugin(
  ("org.typelevel" %% "kind-projector" % KindProjectorVersion).cross(CrossVersion.full),
)

enablePlugins(ScalafmtPlugin, JavaAppPackaging)

// Note: This fixes error with sbt run not loading config properly
fork in run := true
