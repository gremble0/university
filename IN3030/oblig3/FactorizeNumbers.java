import java.util.ArrayList;
import java.util.List;

class FactorizeNumbers {
  private final long n;
  private final int[] primes;
  private final int k, cores;
  private final Oblig3Precode precode;
  private final List<List<Long>> factors;

  // TODO: just do this inside the InInterval class
  private class FactorizeNumber implements Runnable {
    private final long num;
    private List<Long> numFactors;

    FactorizeNumber(long num) {
      this.num = num;
      // TODO: inject index instead of calculating it
      this.numFactors = factors.get((int) (n - num)); // TODO: linked list?
    }

    public void run() {
      int i = 0;
      long numIter = num;

      System.out.println(num);
      while (i < primes.length && primes[i] <= numIter) {
        // If numIter is even we dont need to do expensive modulo operation
        if ((numIter & 1) == 0) {
          numFactors.add(Long.valueOf(2));
          numIter /= 2;
          i = 0;
          continue;
        }

        if (numIter % primes[i] == 0) {
          numFactors.add(Long.valueOf(primes[i]));
          numIter /= i;
          i = 0;
          continue;
        }
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

  public FactorizeNumbers(long n, int k, int[] primes, int cores, Oblig3Precode precode) {
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

  public void writeFactors() {
    for (int i = 0; i < factors.size(); i++) {
      List<Long> factorsForI = factors.get(i);
      for (int j = 0; j < factorsForI.size(); j++) {
        precode.addFactor(n - i, factorsForI.get(j));
      }
    }

    precode.writeFactors();
  }
}
