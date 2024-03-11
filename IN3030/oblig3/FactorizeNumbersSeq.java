import java.util.ArrayList;
import java.util.List;

class FactorizeNumbersSeq {
  private final long n;
  private final int[] primes;
  private final int k;
  private final Oblig3Precode precode;
  private final List<List<Long>> factors;

  public FactorizeNumbersSeq(long n, int k, int[] primes, Oblig3Precode precode) {
    this.n = n;
    this.k = k;
    this.primes = primes;
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
    for (int i = 0; i < k; i++) {
      List<Long> numFactors = factors.get(i);

      int j = 0;
      long numIter = n - i;
      while (j < primes.length && primes[j] <= numIter) {
        // If numIter is even we dont need to do expensive modulo operation
        if ((numIter & 1) == 0) {
          numFactors.add(Long.valueOf(2));
          numIter /= 2;
          j = 0;
          continue;
        }

        // If we have found a factor add it
        if (numIter % primes[j] == 0) {
          numFactors.add(Long.valueOf(primes[j]));
          numIter /= primes[j];
          j = 0;
          continue;
        }

        // If no factor was found move on to the next prime
        ++j;
      }

      // num is a prime
      if (numFactors.size() == 0) {
        numFactors.add(n - i);
      }
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

  public static void main(String[] args) {
    int n, k;

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
    } catch (Exception e) {
      System.out.println("Correct use of program is: java FactorizeNumbers <n:unsigned int> <k?:unsigned int>");
      return;
    }

    int[] primes = new SieveOfEratosthenesSeq(n).getPrimes();

    FactorizeNumbersSeq fn = new FactorizeNumbersSeq(n, k, primes, new Oblig3Precode(n));

    long beforeFactorization = System.nanoTime();
    fn.factorize();
    long afterFactorization = System.nanoTime();

    fn.writeFactors();
    System.out.println("Time to calculate factors: " + (afterFactorization - beforeFactorization));
  }
}
