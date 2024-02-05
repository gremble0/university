import java.util.Arrays;

public class KLargestNumbers {
    public int[] nums;
    public int k;
    private class KLargestInInterval implements Runnable {
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

    public KLargestNumbers(int[] nums, int k) {
        this.nums = nums;
        this.k = k;
    }

    /**
     * Move this.k largest elements of this.nums to its front in place
     * using a sequential algorithm
     */
    public void findKLargestSequential() {
        new KLargestInInterval(0, nums.length).run();
    }

    /**
     * Move this.k largest elements of this.nums to its front in place
     * using a parallel algorithm
     */
    public void findKLargestParallel() {
        final int cores = Runtime.getRuntime().availableProcessors();
        final int intervalSize = nums.length / cores;

        // Algorithm doesnt make sense if k is too close to the size of the array.
        // In this case just sort the whole array instead
        if (k >= intervalSize) {
            insertSortDescending(nums, 0, nums.length - 1);
            return;
        }

        final Thread[] threads = new Thread[cores];

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

        pushLargestToFront(threads.length, intervalSize);
    }

    /**
     * Find this.k largest numbers in each of the given intervals and keep
     * the biggest ones in this.nums[0..k]
     *
     * @param intervals how many intervals there are in this.nums
     * @param intervalSize the distance between each interval in this.nums
     */
    private void pushLargestToFront(int intervals, int intervalSize) {
        int[] biggest = new int[k];
        Arrays.fill(biggest, Integer.MIN_VALUE);

        int start = 0;
        for (int i = 0; i < intervals; i++) {
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
