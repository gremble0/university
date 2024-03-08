import java.util.ArrayList;
import java.util.List;

class SieveOfEratosthenesPara {
  private final int cores;
  private int n, root;
  private int[] numsOfPrimes;
  private byte[] oddNumbers;
  private final Oblig3Precode precode;
  private final List<List<Long>> factors = new ArrayList<>();

  private class FactorizeNumber implements Runnable {
    private final long num;
    private List<Long> numFactors;

    FactorizeNumber(long num) {
      this.num = num;
      this.numFactors = new ArrayList<Long>();
    }

    public void run() {
      int i = 2;
      long numIter = num;

      while (i < numIter) {
        if (num % i == 0) {
          numFactors.add(Long.valueOf(i));
          numIter /= i;
          i = 2;
          continue;
        }

        int nextP = nextPrime(i);
        if (nextP == -1) {
          break;
        } else {
          i = nextP;
        }
      }

      if (numFactors.size() == 0) {
        numFactors.add(num);
      }

      // TODO: is this thread safe?
      factors.add(numFactors);
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
   * Constructor that initializes the global variables
   * 
   * @param n Prime numbers up until (and including if prime) 'n' is found
   */
  SieveOfEratosthenesPara(int n, int threads) {
    this.n = n;
    this.cores = threads;
    this.root = (int) Math.sqrt(n);
    this.oddNumbers = new byte[(n / 16) + 1];
    this.numsOfPrimes = new int[cores];
    this.precode = new Oblig3Precode(n);
  }

  private class SieveInInterval implements Runnable {
    final int start, end, numsOfPrimesIndex;

    public SieveInInterval(int start, int end, int numsOfPrimesIndex) {
      this.start = start;
      this.end = end;
      this.numsOfPrimesIndex = numsOfPrimesIndex;
    }

    public void run() {
      int prime;

      // If start is even we need to add 1 to it before nextPrime will ever find a
      // prime. If start is a prime number nextPrime will skip past it so we need to
      // account for that too.
      if ((start & 1) == 0) {
        prime = isPrime(start + 1) ? start + 1 : nextPrime(start + 1);
      } else {
        prime = isPrime(start) ? start : nextPrime(start);
      }

      while (prime < end && prime != -1) {
        traverse(prime);
        prime = nextPrime(prime);

        ++numsOfPrimes[numsOfPrimesIndex];
      }
    }

    private void traverse(int prime) {
      for (int i = prime * prime; i <= n; i += prime * 2)
        mark(i);
    }
  }

  public List<List<Long>> factorize(int k) {
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

  /**
   * Finds the next prime after some odd number.
   *
   * @param prev the number to start looking for prime numbers after. This must be
   *             an odd number
   * @return the first prime after prev
   */
  private int nextPrime(int prev) {
    for (int i = prev + 2; i <= root; i += 2)
      if (isPrime(i))
        return i;

    return -1;
  }

  public int[] getPrimes() {
    // No point using parallel version for small numbers
    if (root <= cores) {
      SieveOfEratosthenesSeq soe = new SieveOfEratosthenesSeq(n);
      return soe.getPrimes();
    }

    mark(1);

    // Start threads
    final Thread[] threads = new Thread[cores];
    final int intervalSize = root / cores;

    int start = 0;
    for (int i = 0; i < cores - 1; i++) {
      threads[i] = new Thread(new SieveInInterval(start, start + intervalSize, i));
      threads[i].start();

      start += intervalSize;
    }
    // Last thread takes the numbers upto the root of n
    threads[cores - 1] = new Thread(new SieveInInterval(start, root - 1, cores - 1));
    threads[cores - 1].start();

    // Synchronize and gather results
    try {
      for (int i = 0; i < threads.length; i++) {
        threads[i].join();
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

    return collectPrimes();
  }

  private int[] collectPrimes() {
    int start = (root % 2 == 0) ? root + 1 : root + 2;
    int totalNumOfPrimes = 1;
    for (int numOfPrimes : numsOfPrimes) {
      totalNumOfPrimes += numOfPrimes;
    }

    for (int i = start; i <= n; i += 2)
      if (isPrime(i))
        totalNumOfPrimes++;

    int[] primes = new int[totalNumOfPrimes + 1];
    primes[0] = 2;

    int j = 1;
    for (int i = 3; i <= n; i += 2) {
      if (isPrime(i)) {
        primes[j++] = i;
      }
    }

    return primes;
  }

  /**
   * Checks if a number is a prime number. If 'num' is prime, it returns true.
   * If 'num' is composite, it returns false.
   * 
   * @param num The number to check.
   * @return A boolean; true if prime, false if not.
   */
  private boolean isPrime(int num) {
    int bitIndex = (num % 16) / 2;
    int byteIndex = num / 16;

    return (oddNumbers[byteIndex] & (1 << bitIndex)) == 0;
  }

  /**
   * Marks the number 'num' as a composite number (non-prime)
   * 
   * @param num The number to be marked non-prime.
   */
  private void mark(int num) {
    int bitIndex = (num % 16) / 2;
    int byteIndex = num / 16;
    oddNumbers[byteIndex] |= (1 << bitIndex);
  }

  /**
   * Prints the primes found.
   */
  static void printPrimes(int[] primes) {
    for (int prime : primes) {
      System.out.println(prime);
    }
  }

  /**
   * @param args Contains the number up to which we want to find prime numbers and
   *             optionally the amount of threads to assign to the program
   */
  public static void main(String[] args) {
    int n, threads;

    try {
      n = Integer.parseInt(args[0]);
      if (n <= 0) {
        throw new Exception();
      }

      threads = args.length > 1
          ? Integer.parseInt(args[1])
          : Runtime.getRuntime().availableProcessors();
      if (threads <= 0) {
        throw new Exception();
      }
    } catch (Exception e) {
      System.out.println("Correct use of program is: java SieveOfEratosthenesPara " +
          "<n:unsigned int> <threads?: unsigned int>");
      return;
    }

    SieveOfEratosthenesPara soe = new SieveOfEratosthenesPara(n, threads);

    long before = System.nanoTime();
    soe.getPrimes();
    System.out.println(System.nanoTime() - before);

    soe.factorize(100);
    soe.writeFactors();
  }
}
