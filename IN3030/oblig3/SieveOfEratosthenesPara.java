class SieveOfEratosthenesPara {
  /**
   * Declaring all the global variables
   *
   */
  int n, root, numOfPrimes;
  byte[] oddNumbers;

  /**
   * Constructor that initializes the global variables
   * 
   * @param n Prime numbers up until (and including if prime) 'n' is found
   */
  SieveOfEratosthenesPara(int n) {
    this.n = n;
    root = (int) Math.sqrt(n);
    oddNumbers = new byte[(n / 16) + 1];
  }

  /**
   * Performs the sieve and collects the primes produced by the sieve.
   * 
   * @return An array containing all the primes up to and including 'n'.
   */
  int[] getPrimes() {
    if (n <= 1)
      return new int[0];

    sieve();

    return collectPrimes();
  }

  /**
   * Iterates through the array to count the number of primes found,
   * creates an array of that size and populates the new array with the primes.
   * 
   * @return An array containing all the primes up to and including 'n'.
   */
  private int[] collectPrimes() {

    int start = (root % 2 == 0) ? root + 1 : root + 2;

    for (int i = start; i <= n; i += 2)
      if (isPrime(i))
        numOfPrimes++;

    int[] primes = new int[numOfPrimes];

    primes[0] = 2;

    int j = 1;

    for (int i = 3; i <= n; i += 2)
      if (isPrime(i))
        primes[j++] = i;

    return primes;
  }

  /**
   * Performs the Sieve Of Eratosthenes
   */
  private void sieve() {
    mark(1);
    numOfPrimes = 1;
    int prime = nextPrime(1);

    while (prime != -1) {
      traverse(prime);
      prime = nextPrime(prime);
      numOfPrimes++;
    }
  }

  /**
   * Marks all odd number multiples of 'prime', starting from prime * prime.
   * 
   * @param prime The prime used to mark the composite numbers.
   */
  private void traverse(int prime) {
    for (int i = prime * prime; i <= n; i += prime * 2)
      mark(i);
  }

  /**
   * Finds the next prime in the sequence. If there are no more left, it
   * simply returns -1.
   * 
   * @param prev The last prime that has been used to mark all non-primes.
   * @return The next prime or -1 if there are no more primes.
   */
  private int nextPrime(int prev) {
    for (int i = prev + 2; i <= root; i += 2)
      if (isPrime(i))
        return i;

    return -1;
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
   * 
   * @param primes The array containing all the primes.
   */
  static void printPrimes(int[] primes) {
    for (int prime : primes)
      System.out.println(prime);
  }

  /**
   * Expects a positive integer as an argument.
   * 
   * @param args Contains the number up to which we want to find prime numbers.
   */
  public static void main(String[] args) {

    int n;

    try {
      n = Integer.parseInt(args[0]);
      if (n <= 0)
        throw new Exception();
    } catch (Exception e) {
      System.out.println("Correct use of program is: " +
          "java SieveOfEratosthenesPara <n> where <n> is a positive integer.");
      return;
    }

    SieveOfEratosthenesPara soe = new SieveOfEratosthenesPara(n);

    /**
     * Getting all the primes equal to and below 'n'
     */
    int[] primes = soe.getPrimes();

    /**
     * Printing the primes collected
     */
    printPrimes(primes);
  }
}
