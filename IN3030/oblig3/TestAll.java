/*
 * This file tests the sequential and the parallell solution of the Sieve for
 * the given parameters. It verifies that the outputs of the parallell and
 * sequential algorithms are identical and then factorizes the k numbers
 * below n and writes it to a file
 */

class TestAll {
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
      System.out.println("Correct use of program is: java TestAll " +
          "<n:unsigned int> <k?:unsigned int> <threads?: unsigned int>");
      return;
    }

    SieveOfEratosthenesSeq soeS = new SieveOfEratosthenesSeq(n);
    SieveOfEratosthenesPara soeP = new SieveOfEratosthenesPara(n, threads);

    long beforePrimesS = System.nanoTime();
    int[] soeSPrimes = soeS.getPrimes();
    long afterPrimesS = System.nanoTime();

    long beforePrimesP = System.nanoTime();
    int[] soePPrimes = soeP.getPrimes();
    long afterPrimesP = System.nanoTime();

    if (soePPrimes.length != soePPrimes.length) {
      System.out.println(
          "Seq(" + soeSPrimes.length + ") and Para(" + soePPrimes.length + ") have different lengths.");
      return;
    }

    for (int i = 0; i < soeSPrimes.length; i++) {
      if (soePPrimes[i] != soeSPrimes[i]) {
        System.out.println("Found difference at " + i + ": Para - " + soePPrimes[i] +
            " Seq - " + soeSPrimes[i]);
      }
    }

    System.out.println("Sequential sieve time: " + (afterPrimesS -
        beforePrimesS));
    System.out.println("Parallel sieve time:   " + (afterPrimesP - beforePrimesP));

    long beforeFactors = System.nanoTime();
    FactorizeNumbers fn = new FactorizeNumbers(n, k, soePPrimes, threads, new Oblig3Precode(n));
    long afterFactors = System.nanoTime();

    System.out.println("Factorization time:    " + (afterFactors - beforeFactors));

    fn.writeFactors();
  }
}
