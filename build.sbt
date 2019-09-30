import Dependencies._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.github.pureone"
ThisBuild / organizationName := "com/github/pureone"

lazy val root = (project in file("."))
  .settings(
    name := "fpinscala-learning",
    parallelExecution in Test := false,
    libraryDependencies ++= dependencies
  )
