import java.util.concurrent.*;

class WaitNext2 {
    static Semaphore okToKick = new Semaphore(1, true);
    static Semaphore okToEnterHolding = new Semaphore(1, true);
    static Semaphore holdingArea = new Semaphore(0, true);
    static int N = 4; // defaiult number of iterations
    static boolean firstState = true;
    static boolean skipState = false; // Indicates the second state described in the oblig (thread will not wiait when
                                      // in this state)
    static int debuglevel = 9; // 4: varispeed resumption; 3: varispeed delay time; 2: sems values; 1, : (not
                               // implemented)
    static boolean variableSpeedThreads = true;
    static double variableSpeedRate = 100.0; // threads sleep for a random time between 0 and this rate in microseconds
    static int extraSlowThreads = 0; // number of threads that sleep 10x variableSpeedRate

    public static void printSems(int id, int iteration) {
        if (debuglevel > 1)
            System.out.println("Thread " + id + ", " + iteration + "    Sems: KICK " + okToKick.availablePermits() +
                    " Q: " + okToKick.getQueueLength() +
                    "; ENTER " + okToEnterHolding.availablePermits() + " Q: " + okToEnterHolding.getQueueLength() +
                    "; HOLD:" + holdingArea.availablePermits() + " Q: " + holdingArea.getQueueLength());
    }

    public static void variSpeed(int id, int iteration) { // let the calling thread sleep a random time
        long myWait = (long) (Math.random() * variableSpeedRate);
        if (id < extraSlowThreads)
            myWait = (long) (variableSpeedRate * 10.0); // make the first <extraSlowThreads> always wait
                                                        // 10xvariableSpeedRate
        debugPrintln(id, iteration, 3, "         variSpeed delay: " + myWait + " ms");
        if (variableSpeedThreads)
            try {
                TimeUnit.MILLISECONDS.sleep(myWait);
            } catch (Exception e) {
                return;
            }
        ;
        debugPrintln(id, iteration, 4, "         resuming after variSpeed delay");
    }

    public static void debugPrintln(int id, int iteration, int buglevel, String msg) {
        if (debuglevel >= buglevel) { // then print the message
            System.out.println("Thread " + id + ", " + iteration + msg);
            printSems(id, iteration);
        }
    }

    public static void waitNext(int id, int iteration) {
        try {
            TimeUnit.MILLISECONDS.sleep((long) id * 10); // let them start in order.

            debugPrintln(id, iteration, 1, " START waitNext");
            okToKick.acquire(); // right to kick the previous thread out
            debugPrintln(id, iteration, 1, " got right to start KICK");

            variSpeed(id, iteration);

            if (firstState) { // kick previous one out - except first time
                debugPrintln(id, iteration, 1, " I am THE FIRST, so NO KICKING");
                variSpeed(id, iteration);
                firstState = false;
            } else {
                variSpeed(id, iteration);

                debugPrintln(id, iteration, 1, " Not first, so KICK now " +
                        ((holdingArea.getQueueLength() > 0) ? " - seems there IS one"
                                : "- WOW it appears the guy in front is not there yet! Well, so pre-release him!"));
                holdingArea.release(); // Kick out the previous thread! Well actually, if the thread is slow and has as
                                       // yet NOT
                                       // entered the holding area, then give it the right to exit immediately
                debugPrintln(id, iteration, 1, " KICKed one out");

                variSpeed(id, iteration);
            }
            debugPrintln(id, iteration, 2, " release KICKing right");
            okToKick.release(); // we have done our kicking, so let the next one enter
            debugPrintln(id, iteration, 2, " KICKing right released");

            variSpeed(id, iteration);

            debugPrintln(id, iteration, 1,
                    " now entering HOLD " + ((holdingArea.availablePermits() > 0) ? " - seems I have been pre-released"
                            : "- appear that I must wait (might be pre-released later - BEFORE I wait)"));
            holdingArea.acquire(); // my turn to wait - will be released by a future kicker, well, actually, if we
                                   // are slow and the kicker is
                                   // FAST then the kicker will ALREADY have kicked us - this info is reflected in
                                   // the semaphore (it being positive)
            debugPrintln(id, iteration, 1, " GOT KICKED OUT of holding area :-)");

            variSpeed(id, iteration);

        } catch (Exception e) {
            return;
        }
    }

    public static void waitAndSwap(int id, int iteration) {
        try {
            TimeUnit.MILLISECONDS.sleep(id * 10);

            okToKick.acquire();

            variSpeed(id, iteration);

            if (firstState) {
                firstState = false;
                skipState = true;

                variSpeed(id, iteration);
            } else if (skipState) {
                variSpeed(id, iteration);

                skipState = false;
                okToKick.release();

                variSpeed(id, iteration);
                return;
            } else {
                skipState = true;
                holdingArea.release();
            }

            okToEnterHolding.acquire();

            variSpeed(id, iteration);

            okToKick.release();
            holdingArea.acquire();
            okToEnterHolding.release();

            variSpeed(id, iteration);
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
    }

    public static void main(String[] args) {
        int numberofthreads = 3;

        if (args.length < 1) {
            System.out.println(
                    "use: java WaitNextC <number of threads> <num iterations> <debug level> <varispeed> <num of extra slow threads>");
            System.out.println("   only the first arguement, number of threads, is required; defaults are:");
            System.out.println("   iterations: " + N);
            System.out.println("   debugLevel: " + debuglevel);
            System.out.println("   variableSpeedRate: " + variableSpeedRate);
            System.out.println("   extraSlowThreads: " + extraSlowThreads);
            System.exit(0);
        }
        if (args.length >= 1) {
            numberofthreads = Integer.parseInt(args[0]);
            System.out.println("   threads: " + N);
        }
        if (args.length >= 2) {
            N = Integer.parseInt(args[1]);
            System.out.println("   iterations " + N);
        }
        if (args.length >= 3) {
            debuglevel = Integer.parseInt(args[2]);
            System.out.println("   debugLevel: " + debuglevel);
        }
        if (args.length >= 4) {
            variableSpeedRate = (double) Integer.parseInt(args[3]);
            System.out.println("   variableSpeedRate: " + variableSpeedRate);
        }
        if (args.length >= 5) {
            extraSlowThreads = Integer.parseInt(args[4]);
            System.out.println("   extraSlowThreads: " + extraSlowThreads);
        }
        if (variableSpeedRate <= 0.0)
            variableSpeedThreads = false;

        System.out
                .println("*******************************************************************************************");

        System.out.println(
                "NOTE: after the first thread has entered HOLDING, there will always be ONE thread either WAITING or about to WAIT, so the program will NEVER terminate!");

        Thread[] t = new Thread[numberofthreads];

        System.out.println("Number of threads: " + numberofthreads + ";  iterations: " + N + ";  debug: " + debuglevel
                + ";  varispeed: " + ((long) variableSpeedRate) + " ms;  extra slow: " + extraSlowThreads);

        for (int j = 0; j < numberofthreads; j++) {
            (t[j] = new Thread(new Worker(j))).start();
        }

    }

    static class Worker implements Runnable {
        int myId;

        public Worker(int in) {
            myId = in;
        };

        public void run() {
            debugPrintln(myId, 0, 1, " START thread");

            for (int i = 0; i < N; i++) {
                debugPrintln(myId, i, 2, " START iteration");
                waitAndSwap(myId, i);
                debugPrintln(myId, i, 2, " END iteration");
            }
            debugPrintln(myId, 0, 1, " END thread");
        }

    }

}
