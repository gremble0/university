# IN3030 Oblig3 report - hermagst@uio.no
## User guide
All the 4 algorithms have a separate main function, but these were mostly for debugging purposes. To validate the algorithms and generate more meaningful benchmarks please run the TestAll.java file. This file will run all the algorithms. This program takes 3 arguments:
- n: the number of primes to find and the base for factorization
- k: the number of elements below n to factorize(defaults to 100 like in oblig text)
- threads: the number of threads to use for the parallel algorithms(defaults to all available processors).

Example usage:
```sh
$ javac *.java

$ java TestAll 200000000
```

## Parallel Sieve of Eratosthenes
My solution here had a couple iterations. I first tried a more direct mapping from sequential to parallel, with each bit in every byte of the oddNumbers were a flag for whether a number was composite or not. This worked for smaller values of n. However for bigger values i noticed that sometimes the parallel version would skip over a couple primes. I figured this was because simultaneous updating of the same byte would result in a race condition where sometimes one (or more) of the threads changes would be overwritten by the other thread(s). 

After that i tried a very simple ArrayList<Long> approach where i would add all composite numbers to the arraylist and then synchronize with the results at the end. However this resulted in some quite ridiculous memory usage (even allocating > 20GB of ram would cause the program to crash for larger n values).

Finally I arrived at my final solution which is quite similar to the sequential, just using a full byte for each composite number flag. This is quite memory inefficient (7-8x worse than for the sequential), but this seems to be the only way to transfer this version of the algorithm to a parallel version.

I also noticed that a significant portion of the execution time was spent inside the sequential collectPrimes() method, so I experimented with different approaches to this. I ended up with a solution quite similar to the sequential.

## Parallel factorization of large numbers


## Timings
NOTE: primes calculated are for n * 2, not n * n

## Appendix
Following are the terminal outputs of running the program with the different n values specified in the oblig and the other fields left blank. The numbers are also added in a more readable format on the lines above each terminal input.

```console
$ javac *.java

# n = 2 000 000 
$ java TestAll 2000000
Average time to find primes sequential:       6ms
Average time to find primes parallel:         4ms
Average time to calculate factors sequential: 9ms
Average time to calculate factors parallel:   6ms

# n = 20 000 000
$ java TestAll 20000000
Average time to find primes sequential:       64ms
Average time to find primes parallel:         41ms
Average time to calculate factors sequential: 70ms
Average time to calculate factors parallel:   30ms

# n = 200 000 000
$ java TestAll 200000000
Average time to find primes sequential:       634ms
Average time to find primes parallel:         722ms
Average time to calculate factors sequential: 607ms
Average time to calculate factors parallel:   133ms

# n = 2 000 000 000
$ java TestAll 2000000000
Average time to find primes sequential:       10046ms
Average time to find primes parallel:         8536ms
Average time to calculate factors sequential: 3323ms
Average time to calculate factors parallel:   639ms

```




n=2 000 000
Average time to find primes sequential:       6ms
Average time to find primes parallel:         4ms
Average time to calculate factors sequential: 9ms
Average time to calculate factors parallel:   6ms

n=20 000 000
Average time to find primes sequential:       64ms
Average time to find primes parallel:         41ms
Average time to calculate factors sequential: 70ms
Average time to calculate factors parallel:   30ms

n=200 000 000
Average time to find primes sequential:       634ms
Average time to find primes parallel:         722ms
Average time to calculate factors sequential: 607ms
Average time to calculate factors parallel:   133ms

n=2 000 000 000
Average time to find primes sequential:       10046ms
Average time to find primes parallel:         8536ms
Average time to calculate factors sequential: 3323ms
Average time to calculate factors parallel:   639ms
