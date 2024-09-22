object Main {
  def main(args: Array[String]): Unit = {
    println()
  }
  def rankedWords(wordScore: String => Int, word: List[String]): List[String] = {
    def negativeScore(word: String): Int = -wordScore(word)
    words.sortBy(negativeScore)
  }
}
