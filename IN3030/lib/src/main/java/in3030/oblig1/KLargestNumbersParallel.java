import java.util.Arrays;
import java.util.Random;

class KLargestNumbersParallel {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("usage: java KLargestNumbersParallel <int:numsLen> <int:k>");
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
        class KLargestInInterval implements Runnable {
            private final int start;
            private final int end;

            public KLargestInInterval(int start, int end) {
                this.start = start;
                this.end = end;
            }

            public void run() {
                insertSortDesc(nums, start, start + k);

                for (int i = start + k; i < end; i++) {
                    if (nums[i] > nums[start + k]) {
                        nums[start + k] = nums[i];
                        insertSortDesc(nums, start, start + k);
                    }
                }
            }
        }

        final int cores = Runtime.getRuntime().availableProcessors();
        Thread[] threads = new Thread[cores];
        
        final int intervalSize = nums.length / cores;
        int start = 0;
        int end = intervalSize;
        for (int i = 0; i < cores - 1; i++) {
            threads[i] = new Thread(new KLargestInInterval(start, end));
            threads[i].start();

            start += intervalSize;
            end += intervalSize;
        }
        // Last thread searches to the end of the array instead of nums[start..start + intervalSize]
        threads[cores - 1] = new Thread(new KLargestInInterval(start, nums.length));
        threads[cores - 1].start();

        try {
            for (int i = 0; i < threads.length; i++) {
                threads[i].join();
            }
        } catch(Exception e) {
            return;
        }

        start = 0;
        for (int i = 0; i < threads.length; i++) {
            for (int j = start; j < start + k; j++) {
                if (nums[j] > nums[k - 1]) {
                    nums[k - 1] = nums[j];
                    insertSortDesc(nums, 0, k - 1);
                }
            }
            start += intervalSize;
        }

        System.out.println(Arrays.toString(nums));
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
