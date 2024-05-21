import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class TwinPrimes {
    // We make this function return a List<int[]> because we cannot know the
    // size of the output array beforehand so this is more flexible and
    // probably more efficient than trying to work around it. However if we
    // for some reason want to avoid using an ArrayList we could for example
    // loop through the array once to count the amount of pairs, then
    // initialize an array of that size and finally loop through the array
    // again populate the array. We could also convert the `pairs` List to an
    // int[] and return that if we want a different interface.
    public static List<int[]> getPrimePairs(int[] primes) {
        List<int[]> pairs = new ArrayList<>();
        // `primes.length - 1` because we look ahead in each loop iteration
        for (int i = 0; i < primes.length - 1; ++i)
            if (primes[i] == primes[i + 1] - 2)
                pairs.add(new int[] { primes[i], primes[i + 1] });

        return pairs;
    }

    // Alternative solution
    public static List<int[]> getPrimePairs(int[] primes, SieveOfEratosthenes sieve) {
        List<int[]> pairs = new ArrayList<>();

        // kinda breaking the abstraction barrier by accessing the nextPrime method
        // but this method could might as well have been a part of the sieve class
        // anyways, so it's okay.
        int prime = sieve.nextPrime(1);
        while (prime != -1) {
            int nextPrime = sieve.nextPrime(prime);
            if (prime == nextPrime - 2)
                pairs.add(new int[] { prime, nextPrime });

            prime = nextPrime;
        }

        return pairs;
    }

    public static void main(String[] args) {
        int n;
        try {
            n = Integer.parseInt(args[0]);
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }

        // Assume we have this from oblig3, this could be the sequential sieve since
        // the rest of the question is about solving the prime pairs sequentially.
        SieveOfEratosthenes sieve = new SieveOfEratosthenes(n);
        int[] primes = sieve.getPrimes();

        List<int[]> primePairs = getPrimePairs(primes, sieve);

        System.out.println("All prime number pairs up to " + n + ":");
        for (int[] pair : primePairs)
            System.out.println(Arrays.toString(pair));
    }
}
