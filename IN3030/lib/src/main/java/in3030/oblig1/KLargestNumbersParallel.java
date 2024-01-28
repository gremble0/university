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
                System.out.println(this.start);
                for (int i = start; i < end; i++) {
                    if (nums[i] > nums[start + k - 1]) {
                        nums[start + k - 1] = nums[i];
                        insertSortDesc(nums, start, start + k - 1);
                    }
                }
            }
        }

        final int cores = Runtime.getRuntime().availableProcessors();
        Thread[] threads = new Thread[cores];
        
        final int intervalSize = nums.length / cores;
        for (int i = 0; i < cores; i++) {
            final int start = intervalSize * i;
            Thread t = new Thread(new KLargestInInterval(start, start + intervalSize));
            threads[i] = t;
            t.start();
        }

        try {
            for (int i = 0; i < threads.length; i++) {
                threads[i].join();
            }
        } catch(Exception e) {
            return;
        }

        for (int i = 0; i < threads.length; i++) {
            final int start = intervalSize * i;
            // System.out.println(start);
            for (int j = start; j < start + k; j++) {
                if (nums[j] > nums[k - 1]) {
                    nums[k - 1] = nums[j];
                    insertSortDesc(nums, 0, k - 1);
                }
            }
        }

        System.out.println(Arrays.toString(Arrays.copyOf(nums, k)));
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
