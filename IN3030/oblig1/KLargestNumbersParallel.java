import java.util.Arrays;
import java.util.Random;

/**
 * benchmarks ran on AMD Ryzen 5 3600X, 3.8GHz base clock, 6 cores/12 threads,
 * using the linux 'time' command. Example of full output: 
 *      
 *    ❯ time java KLargestNumbersParallel 100000000 100
 *    java KLargestNumbersParallel 100000000 100  1.28s user 0.04s system 225% cpu 0.586 total
 *
 *              A2 parallelized:
 * n = 1000:
 * k = 20:  0.02s user 0.01s system 0.026 total 
 * k = 100: 0.02s user 0.01s system 0.027 total
 *
 * n = 10 000:
 * k = 20:  0.03s user 0.01s system 0.028 total
 * k = 100: 0.20s user 0.03s system 0.044 total
 *
 * n = 100 000:
 * k = 20:  0.04s user 0.02s system 0.032 total
 * k = 100: 0.27s user 0.04s system 0.052 total
 *
 * n = 1 000 000:
 * k = 20:  0.50s user 0.02s system 0.076 total
 * k = 100: 0.75s user 0.02s system 0.100 total
 *
 * n = 10 000 000:
 * k = 20:  0.59s user 0.03s system 0.123 total
 * k = 100: 0.75s user 0.03s system 0.138 total
 *
 * n = 100 000 000:
 * k = 20:  1.11s user 0.05s system 0.570 total
 * k = 100: 1.28s user 0.04s system 0.586 total
 *
 */

class KLargestNumbersParallel {
    static final int cores = Runtime.getRuntime().availableProcessors();
    static Thread[] threads = new Thread[cores];

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("usage: java KLargestNumbersParallel <int:numsLen> <int:k>");
            return;
        }

        final int numsLen = Integer.parseInt(args[0]);
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
     * @param effectiveK number of elements to find
     */
    private static void findKLargest(int[] nums, int k) {
        final int intervalSize = nums.length / cores;
        // Algorithm doesnt make sense if k is too close to the size of the array.
        // In this case just sort the whole array instead
        if (k >= intervalSize) {
            insertSortDescending(nums, 0, nums.length - 1);
            return;
        }

        class KLargestInInterval implements Runnable {
            private final int start;
            private final int end;

            public KLargestInInterval(int start, int end) {
                this.start = start;
                this.end = end;
            }

            public void run() {
                insertSortDescending(nums, start, start + k);

                for (int i = start + k; i < end; i++) {
                    if (nums[i] > nums[start + k]) {
                        nums[start + k] = nums[i];
                        insertSortDescending(nums, start, start + k);
                    }
                }
            }
        }

        int start = 0;
        for (int i = 0; i < cores - 1; i++) {
            threads[i] = new Thread(new KLargestInInterval(start, start + intervalSize));
            threads[i].start();

            start += intervalSize;
        }
        // Last thread searches to the end of the array in case nums.length % cores > 0
        // TODO: make the first core have more to do than the last to optimize
        // (Better if the first thread we start has to iterate over more numbers)
        threads[cores - 1] = new Thread(new KLargestInInterval(start, nums.length - 1));
        threads[cores - 1].start();

        try {
            for (int i = 0; i < threads.length; i++) {
                threads[i].join();
            }
        } catch(Exception e) {
            System.out.println(e);
            System.exit(1);
        }

        int[] biggest = new int[k];
        Arrays.fill(biggest, Integer.MIN_VALUE);

        start = 0;
        for (int i = 0; i < threads.length; i++) {
            for (int j = 0; j < k; j++) {
                if (nums[j + start] > biggest[biggest.length - 1]) {
                    biggest[biggest.length - 1] = nums[j + start];
                    insertSortDescending(biggest, 0, biggest.length - 1);
                }
            }
            start += intervalSize;
        }

        System.arraycopy(biggest, 0, nums, 0, k);
    }

    /**
     * In place insertion sort in descending order within a given range
     *
     * @param nums array to sort
     * @param start lower range
     * @param end upper range
     */
    private static void insertSortDescending(int[] nums, int start, int end) {
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
