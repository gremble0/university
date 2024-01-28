import java.util.Arrays;
import java.util.Random;

/**
 *                         A1:                                A2:                
 * n = 1000:
 * k = 20:  0.01s user 0.02s system 0.028 total | 0.01s user 0.02s system 0.026 total
 * k = 100:            ----||----               | 0.01s user 0.02s system 0.028 total
 *
 * n = 10 000:
 * k = 20:  0.03s user 0.01s system 0.035 total | 0.01s user 0.02s system 0.038 total
 * k = 100:            ----||----               | 0.01s user 0.02s system 0.037 total
 *
 * n = 100 000:
 * k = 20:  0.07s user 0.02s system 0.069 total | 0.02s user 0.02s system 0.041 total 
 * k = 100:            ----||----               | 0.02s user 0.02s system 0.042 total
 *
 * n = 1 000 000:
 * k = 20:  0.19s user 0.02s system 0.141 total | 0.05s user 0.01s system 0.049 total
 * k = 100:            ----||----               | 0.06s user 0.01s system 0.062 total
 *
 * n = 10 000 000:
 * k = 20:  0.85s user 0.02s system 0.814 total | 0.10s user 0.02s system 0.101 total
 * k = 100:            ----||----               | 0.11s user 0.01s system 0.106 total
 *
 * n = 100 000 000:
 * k = 20:  8.86s user 0.12s system 8.797 total | 0.62s user 0.08s system 0.678 total
 * k = 100:            ----||----               | 0.67s user 0.05s system 0.695 total
 *
 */

public class KLargestNumbersSequential {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("usage: java KLargestNumbersSequential <int:numsLen> <int:k>");
            return;
        }

        int numsLen = Integer.parseInt(args[0]);
        int k = Integer.parseInt(args[1]);

        Random r = new Random(7363);
        int[] randNums = new int[numsLen];
        for (int i = 0; i < numsLen; i++) {
            randNums[i] = r.nextInt();
        }

        findKLargest(randNums, k);
    }

    /**
     * Move the k largest elements in an array to its front in place
     *
     * @param nums array to search through
     * @param k number of elements to find
     */
    private static void findKLargest(int[] nums, int k) {
        insertSortDesc(nums, 0, k - 1);
        for (int i = k - 1; i < nums.length - 1; ++i) {
            if (nums[i] > nums[k - 1]) {
                nums[k - 1] = nums[i];
                insertSortDesc(nums, 0, k - 1);
            }
        }
    }

    /**
     * In place insertion sort in descending order within a given range
     *
     * @param nums array to sort
     * @param start lower range
     * @param end upper range
     */
    private static void insertSortDesc(int[] nums, int start, int end) {
        int j, t;
        for (int i = start; i < end; i++) {
            t = nums[i + 1];
            j = i;
            while (j >= start && nums[j] < t) {
                nums[j + 1] = nums[j];
                j--;
            }
            nums[j + 1] = t;
        }
    }
}
