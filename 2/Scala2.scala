object Main {
    def main(args: Array[String]): Unit = {
        // 2.19 practice
        println(TipCalculator.getTipPercentage(List.empty))
        println(TipCalculator.getTipPercentage(List("Alice", "Bob", "Charlie")))
        println(TipCalculator.getTipPercentage(List("Alice", "Bob", "Charlie", "Daniel", "Emily", "Frank")))

        // 2.21 caffee break
        val test = new TestPureFunction();
        println(test.increment(6) == 7)
        println(test.increment(0) == 1)
        println(test.increment(-6) == -5)
        println(test.increment(Integer.MAX_VALUE - 1) == Integer.MAX_VALUE)
        println(test.add(2, 5) == 7)
        println(test.add(-2, 5) == 3)
        println(test.wordScore("Scala") == 3)
        println(test.wordScore("function") == 8)
        println(test.wordScore("") == 0)
        println(test.getTipPercentage(List("Alice", "Bob")) == 10)
        println(test.getTipPercentage(List("Alice", "Bob", "Charlie", "Danny", "Emily", "Wojtek")) == 20)
        println(test.getTipPercentage(List.empty) == 0)
        println(' '.equals(test.getFirstCharacter("")))
        println('t'.equals(test.getFirstCharacter("test")))
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

// 2.21 practice
class TestPureFunction {
    def increment(x: Int): Int = {
        x + 1
    }
    def add(a: Int, b: Int): Int = {
        a + b
    }
    def wordScore(word: String): Int = {
        word.replaceAll("a", "").length
    }
    def getTipPercentage(names: List[String]): Int = {
        if (names.size > 5) 20
        else if (names.size > 0) 10
        else 0
    }
    def getFirstCharacter(s: String): Char = {
        if (s.length > 0) s.charAt(0)
        else ' '
    }
}
