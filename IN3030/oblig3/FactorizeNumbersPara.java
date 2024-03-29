import java.util.ArrayList;
import java.util.List;

class FactorizeNumbersPara {
  private final long n;
  private final int[] primes;
  private final int k, cores;
  private final Oblig3Precode precode;
  private final List<List<Long>> factors;

  public FactorizeNumbersPara(long n, int k, int[] primes, int cores, Oblig3Precode precode) {
    this.n = n;
    this.k = k;
    this.primes = primes;
    this.cores = cores;
    this.precode = precode;
    this.factors = new ArrayList<>(k);

    for (int i = 0; i < k; i++) {
      factors.add(new ArrayList<>());
    }
  }

  private class FactorizeNumber implements Runnable {
    private final long num;
    private final List<Long> numFactors;

    FactorizeNumber(long num) {
      this.num = num;
      this.numFactors = factors.get((int) (n - num));
    }

    public void run() {
      int i = 0;
      long numIter = num;

      while (i < primes.length && primes[i] <= numIter) {
        // If numIter is even we dont need to do expensive modulo operation
        if ((numIter & 1) == 0) {
          numFactors.add(Long.valueOf(2));
          numIter /= 2;
          i = 0;
          continue;
        }

        // If we have found a factor add it
        if (numIter % primes[i] == 0) {
          numFactors.add(Long.valueOf(primes[i]));
          numIter /= primes[i];
          i = 0;
          continue;
        }

        // If no factor was found move on to the next prime
        ++i;
      }

      // num is a prime
      if (numFactors.size() == 0) {
        numFactors.add(num);
      }
    }
  }

  private class FactorizeNumbersInInterval implements Runnable {
    private final Thread[] threads;
    private final int start;

    /**
     * @param precode to log all added factors
     * @param start   the lower number in the range max..max-n
     * @param end     the upper number in the range max..max-n, is higher than start
     */
    FactorizeNumbersInInterval(int start, int end) {
      this.start = start;
      this.threads = new Thread[end - start];
    }

    public void run() {
      for (int i = 0; i < threads.length; i++) {
        threads[i] = new Thread(new FactorizeNumber(n - start - i));
        threads[i].start();
      }

      try {
        for (int i = 0; i < threads.length; i++) {
          threads[i].join();
        }
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * @param k how many numbers below n to factorize
   * @return A list of all the factors found for all of numbers in the range
   *         n-k..n
   */
  public List<List<Long>> factorize() {
    final int intervalSize = k / cores;
    Thread[] threads = new Thread[cores];

    int start = 0;
    for (int i = 0; i < cores - 1; i++) {
      threads[i] = new Thread(new FactorizeNumbersInInterval(start, start + intervalSize));
      threads[i].start();
      start += intervalSize;
    }
    // Last thread takes the rest of the numbers
    threads[cores - 1] = new Thread(new FactorizeNumbersInInterval(start, k));
    threads[cores - 1].start();

    try {
      for (int i = 0; i < cores; i++) {
        threads[i].join();
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

    return factors;
  }

  /**
   * Collects factors and writes it to the file using the precode
   */
  public void writeFactors() {
    for (int i = 0; i < factors.size(); i++) {
      List<Long> factorsForI = factors.get(i);
      for (int j = 0; j < factorsForI.size(); j++) {
        precode.addFactor(n - i, factorsForI.get(j));
      }
    }

    precode.writeFactors();
  }

  /**
   * Tests the parallel factorization of numbers up until n * 2. (also runs the
   * parallel sieve, but does not time this)
   */
  public static void main(String[] args) {
    int n, k, threads;

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
      System.out.println("Correct use of program is: java FactorizeNumbers " +
          "<n:unsigned int> <k?:unsigned int> <threads?: unsigned int>");
      return;
    }

    int[] primes = new SieveOfEratosthenesPara(n, threads).getPrimes();

    FactorizeNumbersPara fn = new FactorizeNumbersPara((long) ((long) n * 2 - 1), k, primes, threads,
        new Oblig3Precode(n));

    long beforeFactorization = System.nanoTime();
    fn.factorize();
    long afterFactorization = System.nanoTime();

    fn.writeFactors();
    System.out.println("Time to calculate factors parallel:   "
        + (afterFactorization / 1000000 - beforeFactorization / 1000000) + "ms");
  }
}
