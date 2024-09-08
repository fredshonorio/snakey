import ScalacOptions._

lazy val v = "0.0.0"

lazy val buildSettings = Seq(
  organization := "net.meshapp",
  version      := v,
  scalaVersion := "2.13.14",
  scalacOptions ++= compilerOptions ++ Seq("-Xlint:-byname-implicit"),
  (assembly / target) := new File("target/fat-jar"),
)

lazy val deps = Seq(
  "co.fs2"   %% "fs2-io" % "3.10.2",
  "org.jline" % "jline"  % "3.26.3",
)

lazy val snakey = (project in file("."))
  .settings(
    buildSettings,
    libraryDependencies ++= deps,
    run / fork                 := true,
    assembly / mainClass       := Some("snakey.Main"),
    assembly / assemblyJarName := "snakey.jar",
  )

Global / onChangedBuildSource := ReloadOnSourceChanges
