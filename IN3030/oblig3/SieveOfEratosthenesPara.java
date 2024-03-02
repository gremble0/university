class SieveOfEratosthenesPara {
  /**
   * Declaring all the global variables
   *
   */
  final int cores;
  int n, root, numOfPrimes;
  byte[] oddNumbers;
  int[] primes;

  private static class SieveInInterval implements Runnable {
    final int start, end;

    public SieveInInterval(int start, int end) {
      this.start = start;
      this.end = end;
    }

    public void run() {
      for (int i = start; i < end; i++) {
        // TODO: make new thread for oddNumbers[i]
      }
    }
  }

  void calculatePrimes() {
    final Thread[] threads = new Thread[cores];
    final int intervalSize = oddNumbers.length / cores;

    int start = 0;
    for (int i = 0; i < cores; i++) {
      threads[i] = new Thread(new SieveInInterval(start, start + intervalSize));
      threads[i].start();

      start += intervalSize;
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
  void printPrimes() {
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

    soe.calculatePrimes();
    soe.printPrimes();
  }
}
