object ScalacOptions {
  val compilerOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    // "-language:existentials",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code", // N.B. doesn't work well with the question-mark hole
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Yrangepos",
    "-Werror",
    "-Wunused",
  )
}
