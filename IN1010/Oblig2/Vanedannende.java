public class Vanedannende extends Legemiddel {
    int styrke;

    public Vanedannende(String navn, int pris, double virkestoff, int styrke) {
        super(navn, pris, virkestoff);
        this.styrke = styrke;
    }

    public int hentVanedannendeStyrke() {
        return styrke;
    }

    @Override // Overskriver Legemiddel sin toString metode
    public String toString() {
        // henter forelder-klassens toString metode og legger paa det som er spesifikt for denne subklassen
        return String.format("%s. %s har en vanedannende styrke paa %d", super.toString(), this.navn, this.hentVanedannendeStyrke());
    }
}