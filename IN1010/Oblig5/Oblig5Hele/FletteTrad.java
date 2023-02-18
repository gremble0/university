public class FletteTrad implements Runnable {
    private Monitor2 monitor;

    public FletteTrad(Monitor2 monitor) {
        this.monitor = monitor;
    }

    @Override
    public void run() {
        while (monitor.hentStoerrelse() > 1) {
            monitor.flett();
        }
    }
}
