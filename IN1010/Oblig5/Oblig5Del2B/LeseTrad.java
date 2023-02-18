public class LeseTrad implements Runnable {
    private String filnavn;
    private Monitor2 monitor;

    public LeseTrad(String filnavn, Monitor2 monitor) {
        this.filnavn = filnavn;
        this.monitor = monitor;
    }

    @Override
    public void run() {
        monitor.settInn(SubsekvensBeholder.lesFraFil(filnavn));
    }
}