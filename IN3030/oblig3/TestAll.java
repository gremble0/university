/*
 * This file tests the sequential and the parallell solution of the Sieve for
 * the given parameters. It verifies that the outputs of the parallell and
 * sequential algorithms are identical and then factorizes the k numbers
 * below n and writes it to a file
 */

class TestAll {
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
      System.out.println(
          "Correct use of program is: java TestAll <n:unsigned int> <k?:unsigned int> <threads?: unsigned int>");
      return;
    }

    // Finding primes below n
    SieveOfEratosthenesSeq soeS = new SieveOfEratosthenesSeq(n);
    SieveOfEratosthenesPara soeP = new SieveOfEratosthenesPara(n, threads);

    long beforePrimesS = System.nanoTime();
    int[] soeSPrimes = soeS.getPrimes();
    long afterPrimesS = System.nanoTime();

    long beforePrimesP = System.nanoTime();
    int[] soePPrimes = soeP.getPrimes();
    long afterPrimesP = System.nanoTime();

    try {
      assertArraysEqual(soeSPrimes, soePPrimes);
    } catch (Exception e) {
      return;
    }

    System.out.println("Sequential sieve time: " + (afterPrimesS / 1000000 - beforePrimesS / 1000000) + "ms");
    System.out.println("Parallel sieve time:   " + (afterPrimesP / 1000000 - beforePrimesP / 1000000) + "ms");

    // Factorization of k numbers below n * 2. Using the prime numbers up until n we
    // can only factorize numbers smaller than n * 2. To factorize the squares of
    // the big numbers specified in the oblig we would need to modify the sieve to
    // use longs instead of ints and then calculate the primes up until (n * n) / 2,
    // or modify the factorization to brute force the factorization instead of using
    // the primes calculated by the sieve. Maybe there is some other way to do it,
    // but not that i could think of.
    FactorizeNumbersSeq fnS = new FactorizeNumbersSeq((long) ((long) n * 2 - 1), k, soePPrimes,
        new Oblig3Precode(n));
    FactorizeNumbersPara fnP = new FactorizeNumbersPara((long) ((long) n * 2 - 1), k, soePPrimes, threads,
        new Oblig3Precode(n));

    long beforeFactorsS = System.nanoTime();
    fnS.factorize();
    long afterFactorsS = System.nanoTime();

    long beforeFactorsP = System.nanoTime();
    fnP.factorize();
    long afterFactorsP = System.nanoTime();

    System.out.println("Sequential factorization time: " + (afterFactorsS / 1000000 - beforeFactorsS / 1000000) + "ms");
    System.out.println("Parallel factorization time:   " + (afterFactorsP / 1000000 - beforeFactorsP / 1000000) + "ms");

    fnP.writeFactors();
  }
}
