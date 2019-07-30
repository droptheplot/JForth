package JForth

import java.io.File

case class Config(name: Option[String] = None,
                  input: Option[Either[String, File]] = None,
                  output: Option[File] = None,
                  parser: ParserLike,
                  compiler: CompilerLike)
