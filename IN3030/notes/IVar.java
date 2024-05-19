import java.util.concurrent.*;

class IVar<T> {
    private final Semaphore getSem;
    private final Semaphore putSem;
    private boolean isSet;
    private T value;

    public IVar() {
        this.getSem = new Semaphore(0);
        this.putSem = new Semaphore(1);
        this.isSet = false;
    }

    public boolean put(T value) {
        try {
            putSem.acquire();
            if (!isSet) {
                this.value = value;
                getSem.release(Integer.MAX_VALUE);
                isSet = true;
                return true;
            } else {
                return false;
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return false;
        } finally {
            putSem.release();
        }
    }

    public T get() {
        try {
            // We dont need to do anything between acquiring and releasing -
            // this is only to block the thread until it gets released by
            // a call to `put`
            getSem.acquire();
            getSem.release();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        return value;
    }

    // Below is just to test the class
    // Worker is unused, but something similar may be useful so keep it
    static class Worker implements Runnable {
        int id;
        IVar<Integer> ivar;

        public Worker(int id, IVar<Integer> ivar) {
            this.id = id;
            this.ivar = ivar;
        }

        public void run() {
            System.out.println(id);
            try {
                if (id == 0) {
                    TimeUnit.SECONDS.sleep(2);
                    ivar.put(1);
                } else {
                    System.out.println(ivar.get());
                }
            } catch (Exception e) {
            }
        }
    }

    public static void main(String[] args) {
        // This main method uses assert which needs to be enabled with the `-ea` command
        // line flag, alternatively we could do some similar logic in an if block and
        // throwing an AssertionError
        IVar<Integer> ivar = new IVar<>();
        int expected = 5;

        Thread t1 = new Thread(() -> {
            try {
                TimeUnit.SECONDS.sleep(2);
            } catch (Exception e) {
            }
            boolean success = ivar.put(expected);
            assert success == true : "First call to `put` failed";
            System.out.println(Thread.currentThread().getName() + " successfully called `put`");
        });

        // Threads 2 and 3 will do the same thing and expect the same output so lets
        // create a shared variable for this. Reason we make two threads is just to test
        // that multiple threads calling get concurrently works as expected.
        Runnable testGet = () -> {
            long before = System.nanoTime();
            Integer i = ivar.get();
            long after = System.nanoTime();
            assert after > before + 2000000000 : "Call to `get` didn't block as expected";
            System.out.println(Thread.currentThread().getName() + " blocked for a reasonable amount of time");
            assert i == expected : "Unexpected value in ivar: " + ivar.value;
            System.out.println(Thread.currentThread().getName() + " got the expected value from `get`: " + expected);
        };

        Thread t2 = new Thread(testGet);
        Thread t3 = new Thread(testGet);

        Thread t4 = new Thread(() -> {
            // Here we assume that `t4` will call `put` after `t1` which is probably always
            // going to be true. However it is technically possible that it could end up
            // before `t1` if the sleep gets interrupted or something makes t1 run
            // criminally slowly. Despite that this test should suffice for most cases.
            try {
                TimeUnit.SECONDS.sleep(5);
            } catch (Exception e) {
            }
            boolean success = ivar.put(10);
            assert success == false : "Second call to `put` succeeded when it shouldn't";
            System.out.println(
                    Thread.currentThread().getName() + " failed to update the value in the ivar (expected behavior)");
        });
        t1.start();
        t2.start();
        t3.start();
        t4.start();

        try {
            t1.join();
            t2.join();
            t3.join();
            t4.join();
        } catch (Exception e) {
        }

        assert ivar.value == expected : "Unexpected value in ivar: " + ivar.value;
        System.out.println("After joining threads ivar still has expected value: " + expected);

        System.out.println("All tests succeeded :)");
    }
}
