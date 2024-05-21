import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class TwinPrimesPara {
    public static List<int[]> getPrimePairs(int[] primes) {
        class TwinPrimesInInterval implements Runnable {
            List<int[]> localPairs;
            final int start;
            final int end;

            TwinPrimesInInterval(int start, int end) {
                this.start = start;
                this.end = end;
                this.localPairs = new ArrayList<>();
            }

            public void run() {
                // NOTE: do not do `i < end - 1` - this will miss some pairs at the
                // end of intervals. May be applicable in other scenarios too
                for (int i = start; i < end; ++i)
                    if (primes[i] == primes[i + 1] - 2)
                        localPairs.add(new int[] { primes[i], primes[i + 1] });
            }
        }

        // Use all avaialable processors
        final int numThreads = Runtime.getRuntime().availableProcessors();
        // The number of indicies each thread will calculate pairs for
        final int intervalSize = primes.length / numThreads;

        // Datastructures for each thread + the global result pair list where
        // we add all the local pair lists after each thread is joined
        List<int[]> pairs = new ArrayList<>();
        TwinPrimesInInterval[] tasks = new TwinPrimesInInterval[numThreads];
        Thread[] threads = new Thread[numThreads];

        int start = 0;
        for (int i = 0; i < numThreads - 1; ++i) {
            tasks[i] = new TwinPrimesInInterval(start, start + intervalSize);
            threads[i] = new Thread(tasks[i]);
            threads[i].start();

            start += intervalSize;
        }
        // Last thread takes the rest of the indicies
        tasks[numThreads - 1] = new TwinPrimesInInterval(start, primes.length - 1);
        threads[numThreads - 1] = new Thread(tasks[numThreads - 1]);
        threads[numThreads - 1].start();

        try {
            for (int i = 0; i < numThreads; ++i) {
                // I prefer doing the synchronization sequentially, since this is
                // usually faster when each thread does approximately the same
                // amount of work. We could alternatively use some other synchronization
                // at the end of each threads `run` method like a `synchronized` method/
                // ReentrantLock/Semaphore, etc. to avoid having this sequential part

                threads[i].join();
                // Add all local pairs from this thread to the global pairs
                for (int[] pair : tasks[i].localPairs)
                    pairs.add(pair);
            }
        } catch (Exception e) {
            return pairs;
        }

        return pairs;
    }

    public static void main(String[] args) {
        int n;
        try {
            n = Integer.parseInt(args[0]);
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }

        // Assume we have this from oblig3, this could be the parallel sieve since
        // the rest of the question is about solving the prime pairs concurrently.
        SieveOfEratosthenesSeq sieve = new SieveOfEratosthenesSeq(n);
        int[] primes = sieve.getPrimes();

        List<int[]> primePairs = getPrimePairs(primes);

        System.out.println("All prime number pairs up to " + n + ":");
        for (int[] pair : primePairs)
            System.out.println(Arrays.toString(pair));
    }
}
