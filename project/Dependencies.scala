import sbt._

object Version {
  final val Scala = "2.11.8"
  final val ScalaCheck = "1.13.0"
  final val Cats = "0.5.0"
  final val ScalaTest = "2.2.6"
  final val FreeK = "0.2.5"
}

object Library {
  val scalaCheck = "org.scalacheck" %% "scalacheck" % Version.ScalaCheck
  val cats = "org.typelevel" %% "cats" % Version.Cats
  val scalaTest = "org.scalatest" %% "scalatest" % Version.ScalaTest
  val freek = "com.projectseptember" %% "freek" % Version.FreeK
}
