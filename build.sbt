ThisBuild / scalaVersion := "3.1.1"


lazy val root = project.in(file("."))
  .dependsOn(labrpc)
  .aggregate(labrpc)

lazy val labrpc = project.settings(
  libraryDependencies ++= {
    val zioVersion = "2.0.0-RC3"
    Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  },
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)


// bugio
// mico