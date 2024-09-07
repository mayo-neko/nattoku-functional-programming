object Main {
    def main(args: Array[String]): Unit = {
        // 2.19 practice
        println(TipCalculator.getTipPercentage(List.empty))
        println(TipCalculator.getTipPercentage(List("Alice", "Bob", "Charlie")))
        println(TipCalculator.getTipPercentage(List("Alice", "Bob", "Charlie", "Daniel", "Emily", "Frank")))
    }
}
// 2.19 practice
object TipCalculator {
    def getTipPercentage(names: List[String]): Int = {
        if (names.size > 5) 20
        else if (names.size > 0) 10
        else 0
    }
}
