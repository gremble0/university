import java.util.Arrays;
import java.util.Random;

public class TestKLargestNumbers {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("usage: java " + TestKLargestNumbers.class.getName() + " <int:numsLen> <int:k>");
            return;
        }

        final int numsLen = Integer.parseInt(args[0]);
        int k = Integer.parseInt(args[1]);

        Random r = new Random(7363);
        int[] nums = new int[numsLen];
        for (int i = 0; i < numsLen; i++) {
            nums[i] = r.nextInt();
        }

        KLargestNumbers kln1 = new KLargestNumbers(Arrays.copyOf(nums, nums.length), k);
        KLargestNumbers kln2 = new KLargestNumbers(Arrays.copyOf(nums, nums.length), k);

        long beforeParallel = System.nanoTime();
        kln2.findKLargestParallel();
        long afterParallel = System.nanoTime();

        long beforeSequential = System.nanoTime();
        kln1.findKLargestSequential();
        long afterSequential = System.nanoTime();

        long beforeSort = System.nanoTime();
        Arrays.sort(nums);
        long afterSort = System.nanoTime();

        // assert all are sorted properly
        for (int i = 0; i < k; i++) {
            if (kln1.nums[i] != kln2.nums[i] || kln2.nums[i] != nums[nums.length - i - 1]) {
                System.out.println("Found diffs at index " + i + ": ");
                System.out.println("Parallel:    " + kln2.nums[i]);
                System.out.println("Sequential:  " + kln1.nums[i]);
                System.out.println("Arrays.sort: " + nums[nums.length - i - 1]);
            }
        }

        System.out.println("A2, parallel:               " + (afterParallel - beforeParallel));
        System.out.println("A2, sequential:             " + (afterSequential - beforeSequential));
        System.out.println("Builtin java Arrays.sort(): " + (afterSort - beforeSort));
    }
}
