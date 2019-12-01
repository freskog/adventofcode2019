name := "adventofcode2019"

version := "0.1"

scalaVersion := "2.13.1"

val zioVersion = "1.0.0-RC17"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-streams" % zioVersion,
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
)