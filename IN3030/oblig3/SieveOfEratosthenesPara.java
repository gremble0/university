/*
 * check if number is odd: num & 1
 *
 * Algorithm:
 * Find primes of:
 * 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
 *
 * - get sqrt of max n sqrt(16) = 4
 * - initialize threads for nums less than sqrt(max(n))
 *    - in this case, one thread for 0..1, 1..2, 2..3, 3..4
 * - based on each num in each thread run sieve to mark upwards
 *   - something about n *= n / n *= 2 ?? to step forwards
 *   - synchronizing? if someone is marking a number already, go next? mark same number twice?
 *
 */

import java.util.concurrent.atomic.AtomicInteger;

class SieveOfEratosthenesPara {
  final int cores;
  int n, root;
  AtomicInteger numOfPrimes;
  byte[] oddNumbers;

  private class SieveInInterval implements Runnable {
    final int start, end;
    int localNumOfPrimes;

    public SieveInInterval(int start, int end) {
      this.start = start;
      this.end = end;
      this.localNumOfPrimes = 0;
    }

    public void run() {
      int prime = firstPrime();

      while (prime < end && prime != -1) {
        traverse(prime);
        prime = nextPrime(prime);
        ++localNumOfPrimes;
      }

      numOfPrimes.addAndGet(localNumOfPrimes);
      System.out.println("LOCALNUMOFPRIMES: " + localNumOfPrimes);
    }

    private int firstPrime() {
      int firstOdd;

      // Make start odd if its even
      if ((start & 1) == 0) {
        firstOdd = start + 1;
      } else {
        firstOdd = start;
      }

      // if the firstOdd is a prime it would be skipped over in nextPrime (int i =
      // prev + 2) so check for this first
      if (isPrime(firstOdd)) {
        return firstOdd;
      }

      return nextPrime(firstOdd);
    }

    private int nextPrime(int prev) {
      for (int i = prev + 2; i <= end; i += 2)
        if (isPrime(i))
          return i;

      return -1;
    }

    private void traverse(int prime) {
      for (int i = prime * prime; i <= n; i += prime * 2)
        mark(i);
    }
  }

  int[] getPrimes() {
    // No point using parallel version for small numbers
    if (root <= cores) {
      SieveOfEratosthenesSeq soe = new SieveOfEratosthenesSeq(n);
      return soe.getPrimes();
    }

    mark(1);
    numOfPrimes.set(1);

    final Thread[] threads = new Thread[cores];
    final int intervalSize = root / cores;

    int start = 0;
    for (int i = 0; i < cores - 1; i++) {
      threads[i] = new Thread(new SieveInInterval(start, start + intervalSize));
      threads[i].start();

      start += intervalSize;
    }
    // Last thread takes the numbers upto the root of n
    threads[cores - 1] = new Thread(new SieveInInterval(start, root - 1));
    threads[cores - 1].start();

    // Synchronize and gather results
    try {
      for (int i = 0; i < threads.length; i++) {
        threads[i].join();
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

    // int[] primes = new int[numOfPrimes.get()];
    System.out.println("NUMOFPRIMES: " + numOfPrimes.get());
    int[] primes = new int[n / 2];
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
   * Constructor that initializes the global variables
   * 
   * @param n Prime numbers up until (and including if prime) 'n' is found
   */
  SieveOfEratosthenesPara(int n, int threads) {
    this.n = n;
    this.cores = threads;
    this.root = (int) Math.sqrt(n);
    this.oddNumbers = new byte[(n / 16) + 1];
    this.numOfPrimes = new AtomicInteger();
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

    printPrimes(soe.getPrimes());
  }
}
