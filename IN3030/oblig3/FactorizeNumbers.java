class FactorizeNumbers {
  private final long n;
  private final int k, intervalSize, cores;
  private final Oblig3Precode precode;

  /**
   * @param n       biggest number to factorize
   * @param k       how many numbers below n to factorize
   * @param cores   how many cores to allocate for the methods of this class
   * @param precode used to track factors
   */
  FactorizeNumbers(long n, int k, int cores, Oblig3Precode precode) {
    this.n = n;
    this.k = k;
    this.cores = cores;
    this.intervalSize = k / cores;
    this.precode = precode;
  }

  public void factorize() {
    Thread[] threads = new Thread[cores];

    int start = 0;
    for (int i = 0; i < cores; i++) {
      threads[i] = new Thread(new FactorizeNumbersInInterval(precode, start, intervalSize));
      threads[i].start();
      start += intervalSize;
    }

    try {
      for (int i = 0; i < cores; i++) {
        threads[i].join();
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

    precode.writeFactors();
  }
}
