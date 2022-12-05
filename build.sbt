name := "AOC2022"

version := "0.1"

scalaVersion := "2.13.10"

Test / testOptions +=
  Tests.Argument(
    TestFrameworks.ScalaCheck,
    "-verbosity",
    "1",
    "-minSuccessfulTests",
    "1000"
  )
libraryDependencies ++= Seq(
  "org.scalactic"     %% "scalactic"       % "3.2.13",
  "org.scalatest"     %% "scalatest"       % "3.2.13"  % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.2.0" % Test,
  "org.scalacheck"    %% "scalacheck"      % "1.14.1"  % Test,
  "org.typelevel"     %% "cats-effect"     % "3.3.12"
)
