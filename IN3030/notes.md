## Random
Speed of light: 299 792 458 m/s

## Cache friendliness of bubblesort
```java
static void bubbleSort(int array[]) {
    int size = array.length;
    for (int i = 0; i < size - 1; i++)
        for (int j = 0; j < size - i - 1; j++)
            if (array[j] > array[j + 1]) {
                int temp = array[j];
                array[j] = array[j + 1];
                array[j + 1] = temp;
            }
}
```
### Good (In general)
- Sequential access to the array.
- Temporal locality: frequently accesses the same elements within a short period of time
- Spatial locality: frequently accesses adjacent elements in array
### Bad (In general)
- Generally inefficient algorithm with O(n^2) complexity (not necessarily bad, but we know bubblesort is inefficient)
- Nested for loop means that after each inner for loop completes it will jump back to the start of the array (jumping around in memory is bad)
- Cache saturation: when array size is larger than the cache size the cache may need to evict data that may be needed later (more overhead for managing cache + future cache misses is bad)
### Bad (This implementation)
- Inefficient inner loop: restarting j at 0 and looping up to size - i - 1 in the inner loop is a cache inefficient way to do the bubblesort algorithm. This solution will result in a lot of jumping around in memory when going from the outer to the inner loop which will likely result in cache misses for larger arrays.
### Improvements (In general)
- Even though a better solution for an algorithm is not necessarily always more cache friendly there is generally a correlation. We can say that this is mostly due to cache saturation. More efficient algorithms do fewer operations for the same result, leading to less cache saturation. This is especially true for larger arrays. Since there is very little inherently cache inefficient with the bubble sort algorithm, we can say that any speedups that maintain the `identity` of the bubble sort algorithm is an improvement.
- Parallelism: If we were to parallelize the algorithm we could reduce the cache saturation. The benefit of this would vary depending on both amount of threads and the size of the array, where larger arrays and more threads are benefitial. The sweetspot here would be if we could splice the array to perfectly fit the size of a cache line for each thread (there is also a sweetspot for a not). In terms of implementing this we would end up with an array where it contains several sorted subarrays. We could then run something like mergesort on these subarrays. This (sort of) maintains the integrity of still being the bubblesort algorithm.
### Improvements (This implementation)
- Switch inner for loop to range from `j = i .. j < size - 1`

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

## Sorting algorithms
```c
void arr_swap(void *arr, int i, int j, size_t type_size) {
  char temp[type_size];

  // Copy contents of a[i] into temporary buffer
  memcpy(temp, arr + i * type_size, type_size);
  // Copy a[j] into a[i]
  memcpy(arr + i * type_size, arr + j * type_size, type_size);
  // Copy contents of temp into a[j]
  memcpy(arr + j * type_size, temp, type_size);
}

void bubblesort(int *arr, int size) {
  char swapped = 0;
  // Loop through each number
  for (int i = 0; i < size; ++i) {
    // Propogate as far down the array as we can one index at a time
    // (We could also use an if test to only swap once, but then we would
    // just have to do more recursive calls instead)
    for (int j = i; j < size - 1 && arr[j] > arr[j + 1]; ++j) {
      arr_swap(arr, j, j + 1, sizeof(int));
      swapped = 1;
    }
  }

  if (swapped)
    // After one full iteration of the array, we know we have moved at least one
    // element to the end of the array, so we can decrement the size for the
    // next recursive call
    bubblesort(arr, size - 1);
}

void insertionsort(int *arr, int size) {
  for (int i = 1; i < size; ++i)
    for (int j = i; j > 0 && arr[j - 1] > arr[j]; --j)
      arr_swap(arr, j - 1, j, sizeof(int));
}

void quicksort_impl(int *arr, int start, int end) {
  // basecase
  if (start >= end)
    return;

  // Choose pivot element (here just use the last element in the array)
  int pivot = arr[end - 1];

  int left_i = start;
  int right_i = end - 2;
  // Increment left_i until arr[left_i] is greater than pivot
  while (left_i < end - 1 && arr[left_i] < pivot)
    ++left_i;
  // Decrement right_i until arr[right_i] is less than pivot
  while (right_i > 0 && arr[right_i] >= pivot)
    --right_i;

  if (left_i < right_i) {
    // If left_i < right_i there are still more elements smaller than the pivot
    // that need to be moved to the front of the array
    arr_swap(arr, left_i, right_i, sizeof(int));
    quicksort_impl(arr, start, end);
  } else {
    // Otherwise we are done with this partition and we continue splitting
    // the array into 2 new partitions and sort them
    arr_swap(arr, left_i, end - 1, sizeof(int)); // swap left with pivot
    quicksort_impl(arr, start, left_i);
    quicksort_impl(arr, left_i + 1, end);
  }
}

void quicksort(int *arr, int size) { quicksort_impl(arr, 0, size); }

void selectionsort(int *arr, int size) {
  // Index of smallest number seen in current iteration
  int min_i = 0;
  for (int i = 0; i < size;) {
    for (int j = i; j < size; ++j)
      if (arr[j] < arr[min_i])
        min_i = j;

    arr_swap(arr, min_i, i, sizeof(int));
    min_i = ++i;
  }
}
```
