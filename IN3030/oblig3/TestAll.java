/*
 * This file verifies that all the algorithms work as intented and also writes the factors to the file `Factors_n.txt`. It then tests the algorithms some amount of times before reporting the average times each algorithm took over each test.
 *
 * NOTE: Running this could take a while for bigger values of n (2-3 minutes for n=2000000000 on my machine). You can change the amount of iterations to test by modifying the TEST_ITERATIONS variable below
 */

class TestAll {
  static int n, k, threads;

  private static void assertArraysEqual(int[] arr1, int[] arr2) throws Exception {
    if (arr1.length != arr2.length) {
      System.out.println("Arrays have different length");
      throw new Exception();
    }

    // Verify parallel and sequential gave same results
    for (int i = 0; i < arr1.length; i++) {
      if (arr1[i] != arr2[i]) {
        System.out.println("Found difference at " + i + ": arr1 - " + arr1[i] + " arr2 - " + arr2[i]);
        throw new Exception();
      }
    }
  }

  /**
   * Run all the algorithms once and add the times taken for each algorithm to the
   * timeSums
   *
   * @param timeSums array of at least size 4, each index will be incremented by
   *                 calling this method.
   */
  private static void testOnce(long[] timeSums) {
    // We need to create new objects to reset the state in each iteration. This will
    // produce a bit of garbage, which could impact runtimes. Thus we will run this
    // method multiple times and take the average
    SieveOfEratosthenesSeq soeSeq = new SieveOfEratosthenesSeq(n);
    SieveOfEratosthenesPara soePara = new SieveOfEratosthenesPara(n, threads);

    long beforePrimesSeq = System.nanoTime();
    int[] soeSeqPrimes = soeSeq.getPrimes();
    long afterPrimesSeq = System.nanoTime();

    long beforePrimesPara = System.nanoTime();
    int[] soeParaPrimes = soePara.getPrimes();
    long afterPrimesPara = System.nanoTime();

    FactorizeNumbersSeq facSe = new FactorizeNumbersSeq((long) ((long) n * 2 - 1), k, soeSeqPrimes, null);
    FactorizeNumbersPara facPa = new FactorizeNumbersPara((long) ((long) n * 2 - 1), k, soeParaPrimes, threads, null);

    long beforeFactorsS = System.nanoTime();
    facSe.factorize();
    long afterFactorsS = System.nanoTime();

    long beforeFactorsP = System.nanoTime();
    facPa.factorize();
    long afterFactorsP = System.nanoTime();

    timeSums[0] += (afterPrimesSeq - beforePrimesSeq) / 1000000;
    timeSums[1] += (afterPrimesPara - beforePrimesPara) / 1000000;
    timeSums[2] += (afterFactorsS - beforeFactorsS) / 1000000;
    timeSums[3] += (afterFactorsP - beforeFactorsP) / 1000000;
  }

  public static void main(String[] args) {
    try {
      n = Integer.parseInt(args[0]);
      if (n <= 0) {
        throw new Exception();
      }

      // If argument is omitted set k to 100 as in oblig text
      k = args.length > 1
          ? Integer.parseInt(args[1])
          : 100;
      if (k <= 1) {
        throw new Exception();
      }

      // If argument is omitted take all available processors
      threads = args.length > 2
          ? Integer.parseInt(args[2])
          : Runtime.getRuntime().availableProcessors();
      if (threads <= 0) {
        throw new Exception();
      }
    } catch (Exception e) {
      System.out.println(
          "Correct use of program is: java TestAll <n:unsigned int> <k?:unsigned int> <threads?: unsigned int>");
      return;
    }

    // Initialize objects and run one dry run without benchmarking to verify results
    // and warm up JVM
    SieveOfEratosthenesSeq soeSe = new SieveOfEratosthenesSeq(n);
    SieveOfEratosthenesPara soePa = new SieveOfEratosthenesPara(n, threads);

    int[] soeSPrimes = soeSe.getPrimes();
    int[] soePPrimes = soePa.getPrimes();

    try {
      assertArraysEqual(soeSPrimes, soePPrimes);
    } catch (Exception e) {
      return;
    }

    // Using the prime numbers up until n we can only factorize numbers smaller than
    // n * 2. To factorize the squares of the big numbers specified in the oblig we
    // would need to modify the sieve to use longs instead of ints and then
    // calculate the primes up until (n * n) / 2, or modify the factorization to
    // brute force the factorization instead of using the primes calculated by the
    // sieve. Maybe there is some other way to do it, but not that i could think of.
    FactorizeNumbersPara facPa = new FactorizeNumbersPara((long) ((long) n * 2 - 1), k, soePPrimes, threads,
        new Oblig3Precode(n));

    facPa.factorize();
    facPa.writeFactors();

    // Change to modify accuracy of test benchmarks
    final int TEST_ITERATIONS = 5;
    long[] timeSums = new long[4]; // sums for times for each algorithm, incremented by testOnce()

    for (int i = 0; i < TEST_ITERATIONS; i++) {
      testOnce(timeSums);
    }

    System.out.println("Average time to find primes sequential:       " + timeSums[0] / TEST_ITERATIONS + "ms");
    System.out.println("Average time to find primes parallel:         " + timeSums[1] / TEST_ITERATIONS + "ms");
    System.out.println("Average time to calculate factors sequential: " + timeSums[2] / TEST_ITERATIONS + "ms");
    System.out.println("Average time to calculate factors parallel:   " + timeSums[3] / TEST_ITERATIONS + "ms");
  }
}
