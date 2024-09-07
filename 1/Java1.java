public class M1 {
    public static void main(String[] args) {
        // 1.7 caffee break
        System.out.println(calculateScore("imperative") == 9);
        System.out.println(calculateScore("no") == 2);
        System.err.println(wordScore("declarative") == 9);
        System.err.println(wordScore("yes") == 3);
    }
    // 1.7 caffee break
    public static int calculateScore(String word) {
        int score = 0;
        for (char c: word.toCharArray()) {
            if (c == 'a') continue;
            score++;
        }
        return score;
    }
    public static int wordScore(String word) {
        return word.replace("a", "").length();
    }
}
