public class Subsekvens {
    public Subsekvens(String subsek, int ant) {
        subsekvens = subsek;
        antall = ant;
    }

    private final String subsekvens;
    private int antall;

    // endrer antall forekomster til ny verdi
    public void endreAntall(int nyttAntall) {
        this.antall = nyttAntall;
    }

    // henter antall forekomster
    public int hentAntall() {
        return this.antall;
    }

    @Override
    public String toString() {
        return String.format("(%s,%d)", this.subsekvens, this.antall);
    }
}