object Tokenizer {
  def apply(source: String): Seq[String] = source.split(" +")
}
