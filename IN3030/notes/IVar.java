import java.util.concurrent.*;

class IVar<T> {
    private final Semaphore getSem;
    private final Semaphore putSem;
    private boolean isSet;
    private T value;

    public IVar() {
        this.getSem = new Semaphore(0);
        this.putSem = new Semaphore(1);
        isSet = false;
    }

    public boolean put(T value) {
        try {
            putSem.acquire();
            if (!isSet) {
                this.value = value;
                getSem.release(Integer.MAX_VALUE);
                isSet = true;
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } finally {
            putSem.release();
        }

        return isSet;
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
        IVar<Integer> ivar = new IVar<>();

        Thread t1 = new Thread(() -> {
            try {
                TimeUnit.SECONDS.sleep(2);
            } catch (Exception e) {
            }
            boolean success = ivar.put(5);
            // assert needs to be enabled with the `-ea` flag , alternatively we could
            // do some similar logic in an if block and throwing an AssertionError()
            assert success == true : "First call to `put` failed";
        });
        Thread t2 = new Thread(() -> {
            long before = System.nanoTime();
            Integer i = ivar.get();
            long after = System.nanoTime();
            assert after > before + 2000000000 : "Call to `get` didn't block as expected";
            assert i == 5;
        });
        Thread t3 = new Thread(() -> {
            long before = System.nanoTime();
            Integer i = ivar.get();
            long after = System.nanoTime();
            assert after > before + 2000000000 : "Call to `get` didn't block as expected";
            assert i == 5;
        });
        Thread t4 = new Thread(() -> {
            try {
                TimeUnit.SECONDS.sleep(5);
            } catch (Exception e) {
            }
            boolean success = ivar.put(10);
            assert success == false : "Second call to `put` succeeded when it shouldn't";
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
    }
}
