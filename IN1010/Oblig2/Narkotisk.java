public class Narkotisk extends Legemiddel {
    private int styrke;

    public Narkotisk(String navn, int pris, double virkestoff, int styrke) {
        super(navn, pris, virkestoff);
        this.styrke = styrke;
    }

    public int hentNarkotiskStyrke() {
        return styrke;
    }

    @Override // Overskriver Legemiddel sin toString metode
    public String toString() {
        // henter forelder-klassens toString metode og legger paa det som er spesifikt for denne subklassen
        return String.format("%s. %s har en narkotisk styrke paa %d", super.toString(), this.navn, this.hentNarkotiskStyrke());
    }
}