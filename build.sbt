ThisBuild / scalaVersion := "3.1.1"

lazy val zioSettings: Seq[Def.Setting[_]] = Seq(
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


lazy val root = project.in(file("."))
  .dependsOn(mico, tatu)
  .aggregate(mico, tatu)

lazy val mico = project.settings(zioSettings)
lazy val tatu = project.settings(zioSettings)