// import java.util.ArrayList;
// import java.util.List;
//
// class FactorizeNumbers {
// private final long n;
// private final int k, intervalSize, cores;
// private final Oblig3Precode precode;
// private final List<List<Long>> factors = new ArrayList<>();
//
// private class FactorizeNumber implements Runnable {
// private long num;
// private List<Long> numFactors;
//
// FactorizeNumber(long num) {
// this.num = num;
// this.numFactors = new ArrayList<Long>();
// }
//
// public void run() {
// long i = 2;
// while (i < num) {
// System.out.println(i + " " + num);
// if (num % i == 0) {
// numFactors.add(Long.valueOf(i));
// num /= i;
// i = 2;
// } else {
// ++i;
// // TODO: nextPrime
// // i = nex
// }
// }
//
// // TODO: is this thread safe?
// factors.add(numFactors);
// }
// }
//
// private class FactorizeNumbersInInterval implements Runnable {
// private final Thread[] threads;
// private final int start, end;
//
// /**
// * @param precode to log all added factors
// * @param start the lower number in the range max..max-n
// * @param end the upper number in the range max..max-n, is higher than start
// */
// FactorizeNumbersInInterval(int start, int end) {
// this.start = start;
// this.end = end;
// this.threads = new Thread[end - start];
// }
//
// public void run() {
// for (int i = 0; i < threads.length; i++) {
// threads[i] = new Thread(new FactorizeNumber(n - start - i));
// threads[i].start();
// }
//
// try {
// for (int i = 0; i < threads.length; i++) {
// threads[i].join();
// }
// } catch (Exception e) {
// e.printStackTrace();
// }
// }
// }
//
// /**
// * @param n biggest number to factorize
// * @param k how many numbers below n to factorize
// * @param cores how many cores to allocate for the methods of this class
// * @param precode used to track factors
// */
// FactorizeNumbers(long n, int k, int cores, Oblig3Precode precode) {
// this.n = n;
// this.k = k;
// this.cores = cores;
// this.intervalSize = k / cores;
// this.precode = precode;
// }
//
// public List<List<Long>> factorize() {
// Thread[] threads = new Thread[cores];
//
// int start = 0;
// for (int i = 0; i < cores - 1; i++) {
// threads[i] = new Thread(new FactorizeNumbersInInterval(start, start +
// intervalSize));
// threads[i].start();
// start += intervalSize;
// }
// // Last thread takes the rest of the numbers
// threads[cores - 1] = new Thread(new FactorizeNumbersInInterval(start, k));
// threads[cores - 1].start();
//
// try {
// for (int i = 0; i < cores; i++) {
// threads[i].join();
// }
// } catch (Exception e) {
// e.printStackTrace();
// }
//
// return factors;
// }
//
// public void writeFactors() {
// for (int i = 0; i < factors.size(); i++) {
// List<Long> factorsForI = factors.get(i);
// for (int j = 0; j < factorsForI.size(); j++) {
// precode.addFactor(n - i, factorsForI.get(j));
// }
// }
//
// precode.writeFactors();
// }
// }
