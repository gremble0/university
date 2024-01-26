package in3030.oblig1;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.Random;

class SortSequentialTest {
    @Test
    void sortNumbersSequential() {
        String[] args = { "1000000", "100" };
        int numsLen = Integer.parseInt(args[0]);

        int[] randNums = new int[numsLen];
        Random r = new Random(7363);
        for (int i = 0; i < numsLen; i++) {
            randNums[i] = r.nextInt();
        }
        int[] randNums2 = Arrays.copyOf(randNums, numsLen);

        Arrays.sort(randNums);
        SortSequential.main(args);

        int[] a = {};
        int[] b = {};
        assertArrayEquals(a, b);
        assertArrayEquals(randNums, randNums2);
    }
}
