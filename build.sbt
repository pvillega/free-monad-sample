lazy val freeMonad = project
  .copy(id = "Free-Monad")
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin, GitVersioning)

name := "Free-Monad"

libraryDependencies ++= Vector(
  Library.cats,
  Library.freek,
  Library.scalaCheck % "test",
  Library.scalaTest % "test"
)

initialCommands := """|import com.perevillega.freemonad._
                      |""".stripMargin

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)