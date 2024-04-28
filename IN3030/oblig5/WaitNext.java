import java.util.concurrent.Semaphore;

class WaitNext {
    static Semaphore okToKick = new Semaphore(1);
    static Semaphore okToEnterHoldingArea = new Semaphore(1);
    static Semaphore holdingArea = new Semaphore(1);
    static boolean first = true;
    static int i = 0;
    static int cores = Runtime.getRuntime().availableProcessors();

    static class Worker implements Runnable {
        public void run() {
            waitNext();
        }
    }

    public static void waitNext() {
        ++i;
        System.out.println(i);

        try {
            okToKick.acquire();
            okToEnterHoldingArea.acquire();

            if (first)
                first = false;
            else
                holdingArea.release();

            okToKick.release();
            holdingArea.acquire();
            okToEnterHoldingArea.release();
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
    }

    public static void main(String[] args) {
        Thread[] ts = new Thread[cores];
        for (int i = 0; i < cores; i++)
            (ts[i] = new Thread(new Worker())).run();

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
