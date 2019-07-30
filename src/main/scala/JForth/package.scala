import cats.effect.{ExitCode, IO}

package object JForth {
  implicit class IOUtils(io: IO[_]) {
    def toExitCode: IO[ExitCode] = io.map {
      case Left(_)  => ExitCode.Error
      case Right(_) => ExitCode.Success
    }
  }
}
