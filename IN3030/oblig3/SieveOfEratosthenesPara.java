import java.util.ArrayList;
import java.util.List;

class SieveOfEratosthenesPara {
  private final int cores;
  private final int n, root;
  private final int[] oddNumbers;

  /**
   * Constructor that initializes the global variables
   * 
   * @param n       Prime numbers up until (and including if prime) 'n' is found
   * @param threads amount of threads to allocate for this class
   */
  SieveOfEratosthenesPara(int n, int threads) {
    this.n = n;
    this.cores = threads;
    this.root = (int) Math.sqrt(n);
    this.oddNumbers = new int[(n / 2) + 1];
  }

  private class SieveInInterval implements Runnable {
    final int start, end;

    public SieveInInterval(int start, int end) {
      this.start = start;
      this.end = end;
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

      while (prime <= end && prime != -1) {
        traverse(prime);
        prime = nextPrime(prime);
      }
    }

    private void traverse(int prime) {
      for (int i = prime * prime; i <= n; i += prime * 2)
        mark(i);
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
      threads[i] = new Thread(new SieveInInterval(start, start + intervalSize));
      threads[i].start();

      start += intervalSize;
    }
    // Last thread takes the numbers upto the root of n
    threads[cores - 1] = new Thread(new SieveInInterval(start, root));
    threads[cores - 1].start();

    // Synchronize and gather results
    try {
      for (int i = 0; i < threads.length; i++) {
        threads[i].join();
      }
      System.out.println(oddNumbers.length);
    } catch (Exception e) {
      e.printStackTrace();
    }

    return collectPrimes();
  }

  private int[] collectPrimes() {
    List<Integer> primes = new ArrayList<>();
    primes.add(2);

    for (int i = 3; i <= n; i += 2)
      if (isPrime(i))
        primes.add(i);

    return primes.stream().mapToInt(i -> i).toArray();
  }

  /**
   * Checks if a number is a prime number. If 'num' is prime, it returns true.
   * If 'num' is composite, it returns false.
   * 
   * @param num The number to check.
   * @return A boolean; true if prime, false if not.
   */
  private boolean isPrime(int num) {
    return (oddNumbers[num / 2] == 0);
  }

  /**
   * Marks the number 'num' as a composite number (non-prime)
   * 
   * @param num The number to be marked non-prime.
   */
  private void mark(int num) {
    oddNumbers[num / 2] = 1;
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

    long beforePrimes = System.nanoTime();
    soe.getPrimes();
    long afterPrimes = System.nanoTime();

    System.out.println("Time to calculate primes:  " + (afterPrimes - beforePrimes));
  }
}
