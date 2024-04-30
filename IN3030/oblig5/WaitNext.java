import java.util.concurrent.Semaphore;

class WaitNext {
    static final int n = 10;
    static boolean firstState = true;
    static boolean skipState = false; // Indicates the second state described in the oblig (thread will not wiait when
                                      // in this state)
    static int cores = Runtime.getRuntime().availableProcessors();
    static Semaphore okToKick = new Semaphore(1, true);
    static Semaphore okToEnterHolding = new Semaphore(1, true);
    static Semaphore holdingArea = new Semaphore(0, true);

    static class Worker implements Runnable {
        final int i;

        public Worker(int i) {
            this.i = i;
        }

        public void run() {
            System.out.println("START " + i);
            for (int i = 0; i < n; i++)
                waitAndSwap();

            System.out.println("END " + i);
        }
    }

    public static void printSems() {
        System.out.println("Sems: KICK " + okToKick.availablePermits() +
                " Q: " + okToKick.getQueueLength() +
                "; ENTER " + okToEnterHolding.availablePermits() + " Q: " + okToEnterHolding.getQueueLength() +
                "; HOLD:" + holdingArea.availablePermits() + " Q: " + holdingArea.getQueueLength());
    }

    public static void waitNext() {
        printSems();

        try {
            okToKick.acquire();

            if (firstState)
                firstState = false;
            else
                holdingArea.release();

            okToEnterHolding.acquire();
            okToKick.release();
            holdingArea.acquire();
            okToEnterHolding.release();
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
    }

    public static void waitAndSwap() {
        printSems();

        try {
            okToKick.acquire();

            if (firstState) {
                firstState = false;
                skipState = true;
            } else if (skipState) {
                skipState = false;
                okToKick.release();
                return;
            } else {
                skipState = true;
                holdingArea.release();
            }

            okToEnterHolding.acquire();
            okToKick.release();
            holdingArea.acquire();
            okToEnterHolding.release();
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
    }

    public static void main(String[] args) {
        Thread[] ts = new Thread[cores];
        for (int i = 0; i < cores; i++)
            (ts[i] = new Thread(new Worker(i))).start();

        for (Thread thread : ts) {
            try {
                thread.join();
            } catch (Exception e) {
                e.printStackTrace();
                return;
            }
        }
    }
}
