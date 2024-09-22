import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Java4 {
    public static void main(String[] args) {
        List<String> words = new ArrayList<String>(Arrays.asList("jaskell", "rust", "scala", "java", "ada"));
        System.out.println(rankedWords(w -> score(w) + bonus(w) - penalty(w), words));
    }
    static List<String> rankedWords(Function<String, Integer> wordScore, List<String> words) {
        Comparator<String> wordComparator = (w1, w2) -> Integer.compare(wordScore.apply(w2), wordScore.apply(w1));
        return words.stream()
                    .sorted(wordComparator)
                    .collect(Collectors.toList());
    }

    static int score(String word) {
        return word.replaceAll("a", "").length();
    }
    static int bonus(String word) {
        return word.contains("c") ? 5 : 0;
    }
    static int penalty(String word) {
        return word.contains("s") ? 7 : 0;
    }
}
