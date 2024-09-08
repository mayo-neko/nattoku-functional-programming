import java.util.ArrayList;
import java.util.List;

public class Java2 {
    public static void main(String[] args) {

    }
    // 2.10 coffee break
    class TipCalculator {
        private List<String> names = new ArrayList<>();
        private int tipPercentage = 0;

        public void addPerson(String name) {
            names.add(name);
            if (names.size() > 5) {
                tipPercentage = 20;
            } else if (names.size() > 0) {
                tipPercentage = 10;
            }
        }

        public List<String> getNames() {
            return names;
        }

        public int tipPercentage() {
            return tipPercentage;
        }
    }
    class FixTipCalculator {
        public static int TipCalculator(List<String> names) {
            if (names.size() > 5) {
                return 20;
            } else if (names.size() > 0) {
                return 10;
            } else {
                return 0;
            }
        }
    }
}
