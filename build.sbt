lazy val noIdea = project
  .copy(id = "Free-Monad")
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin, GitVersioning)

name := "Free-Monad"

libraryDependencies ++= Vector(
  Library.scalaCheck % "test"
)

initialCommands := """|import com.perevillega.freemonad._
                      |""".stripMargin
