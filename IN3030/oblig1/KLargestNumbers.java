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
            insertSortDescending(start, start + k);

            for (int i = start + k; i < end; i++) {
                if (nums[i] > nums[start + k - 1]) {
                    insertLargeNumber(start, k, nums[i]);
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
     * using a sequential algorithm (KLargestInInterval.run on the entire array)
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
            insertSortDescending(0, nums.length - 1);
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
        for (int i = 1; i < intervals; i++) {
            int start = i * intervalSize;

            for (int j = 0; j < k && start + j < nums.length; j++) {
                int pos = start + j;

                if (nums[pos] > nums[k - 1]) {
                    int temp = nums[pos];
                    int insertPos = k - 1;

                    while (insertPos > 0 && nums[insertPos - 1] < temp) {
                        nums[insertPos] = nums[insertPos - 1];
                        insertPos--;
                    }
                    nums[insertPos] = temp;
                }
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
    private void insertSortDescending(int start, int end) {
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

    /**
     * Insertion sort type of algorithm for inserting one value into this.nums[start..start + k]
     *
     * @param value what to insert
     */
    private void insertLargeNumber(int start, int k, int value) {
        nums[start + k - 1] = value;
        int i = start + k - 2;
        while (i >= start && nums[i] < value) {
            int at_i = nums[i];
            nums[i] = nums[i + 1];
            nums[i + 1] = at_i;
            --i;
        }
    }
}
