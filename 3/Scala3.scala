object Main {
  def main(args: Array[String]): Unit = {
    println(abbreviate("Alonzo Church"))
    println(abbreviate("A. Church"))
    println(abbreviate("A Church"))
  }

  def abbreviate(fullName: String): String = {
    fullName.charAt(0) + "." + fullName.slice(fullName.indexOf(" "), fullName.length())
  }
}
