import sbt._

object Dependencies {

  def dependencies: Seq[ModuleID] = {
    typesafe ++ scalaTest
  }

  lazy val typesafe = Seq(
    "com.typesafe" % "config" % "1.3.2"
  )

  lazy val scalaTest = Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
  )
}
