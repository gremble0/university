## Random
Speed of light: 299 792 458 m/s

## Cache friendliness of bubblesort
### Good
- Sequential access to the array.
- Temporal locality: frequently accesses the same elements within a short period of time
- Spatial locality: frequently accesses adjacent elements in array
### Bad
- Generally inefficient algorithm with O(n^2) complexity (not necessarily bad, but we know bubblesort is inefficient)
- Nested for loop means that after each inner for loop completes it will jump back to the start of the array (jumping around in memory is bad)
- Cache saturation: when array size is larger than the cache size the cache may need to evict data that may be needed later (more overhead for managing cache + future cache misses is bad)
### Improvements
- Even though a better solution for an algorithm is not necessarily always more cache friendly there is generally a correlation. We can say that this is mostly due to cache saturation. More efficient algorithms do fewer operations for the same result, leading to less cache saturation. This is especially true for larger arrays. Since there is very little inherently cache inefficient with the bubble sort algorithm, we can say that any speedups that maintain the `identity` of the bubble sort algorithm is an improvement.
- Parallelism: If we were to parallelize the algorithm we could reduce the cache saturation. The benefit of this would vary depending on both amount of threads and the size of the array, where larger arrays and more threads are benefitial. The sweetspot here would be if we could splice the array to perfectly fit the size of a cache line for each thread (there is also a sweetspot for a not). In terms of implementing this we would end up with an array where it contains several sorted subarrays. We could then run something like mergesort on these subarrays. This (sort of) maintains the integrity of still being the bubblesort algorithm.

## Threads
- Can threads spawned by the main thread spawn new threads? - Yes because there is nothing special about the main thread except for the fact that it is the thread that gets assigned to do the `main` function. Any thread can spawn another thread.
- How can 100 java threads appear to execute concurrently on a 4 core machine? - This is up to the JVM and the operating system's scheduler to schedule time slices for each of the running threads so that they all get assigned time to execute their tasks. The operating system will also have to manage context switches between the different threads. In practice this means that the threads will regularly have to temporarily pause their execution to allow other threads access to the physical hardware. Even if you were to only create 4 java threads on a 4 core machine, it is unlikely that this would lead to true uninterrupted physical concurrency as there is almost always some other program running on the system that needs to do work on the processor which would have to interrupt the threads in your java program.

## `synchronized`
- Pros and cons of some alternative to `synchronized` - My actual favorite alternative to synchronized would be to avoid it, because using it or any of its alternatives is usually a lot slower than storing data locally in each thread and then later sequentially synchronizing the data after the threads have finished. However if I were to use an alternative to synchronized I would say a lock like javas `ReentrantLock` is quite nice. 
    - Pros:
        - As opposed to synchronized locks allows us to localize the synchronization to the lines where its needed instead of locking the entire function when its called. This gives the programmer more control over the program.
        - More flexible methods for handling interruptions. Also allows you to check if a lock is being held.
        - Can be fair (First In First Out) if fairness parameter is set to true.
    - Cons:
        - Harder to implement, more prone to bugs and errors.
        - Needs to be imported
        - Needs try/finally

## Semaphores
### JoinP - join using Semaphores
```java
import java.util.concurrent.*;

// Worse version:
// Three semaphores. `running` semaphore which is effectively a counter for
// how many threads are currently running. `mainSem` the semaphore for the
// main thread, releasing this is a signal for the main thread to continue.
// `checkRunning` which is a semaphore to stop race conditions when checking
// how many permits are available in `running`.
class JoinP {
    static final int numberofthreads = 10;
    static Semaphore running = new Semaphore(numberofthreads);
    static Semaphore checkRunning = new Semaphore(1); // In most cases unnecessary, but to remove all possible race
                                                      // conditions I added this
    static Semaphore mainSem = new Semaphore(1);

    public static void main(String[] args) {
        Thread[] t = new Thread[numberofthreads];

        for (int j = 0; j < numberofthreads; j++) {
            (t[j] = new Thread(new ExThread())).start();
        }

        try {
            mainSem.acquire();
        } catch (Exception e) {
            return;
        }
    }

    static class ExThread implements Runnable {
        public void run() {
            try {
                running.acquire();
                TimeUnit.SECONDS.sleep(10);
                running.release();

                checkRunning.acquire();
                // If all threads are done release main thread
                if (running.availablePermits() == numberofthreads)
                    mainSem.release();
                checkRunning.release();
            } catch (Exception e) {
                return;
            }
        }
    }
}

// Improved version:
// One semaphore. The main thread acquires the semaphore`numberOfThreads` times
// meaining it can't continue until the semaphore has been released `numberOfThreads`
// times. Effectively making it `join` until all threads are finished
class JoinP {
    static final int numberOfThreads = 10;
    static Semaphore s = new Semaphore(0);

    public static void main(String[] args) {
        Thread[] threads = new Thread[numberOfThreads];

        for (int j = 0; j < numberOfThreads; j++) {
            (threads[j] = new Thread(new ExThread())).start();
        }

        try {
            s.acquire(numberOfThreads);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return;
        }
    }

    static class ExThread implements Runnable {
        public void run() {
            try {
                TimeUnit.SECONDS.sleep(10);
                s.release();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
```
