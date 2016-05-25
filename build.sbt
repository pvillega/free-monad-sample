lazy val freeMonad = project
  .copy(id = "Free-Monad")
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin, GitVersioning)

name := "Free-Monad"

libraryDependencies ++= Vector(
  Library.cats,
  Library.scalaCheck % "test",
  Library.scalaTest % "test"
)

initialCommands := """|import com.perevillega.freemonad._
                      |""".stripMargin

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")