abstract public class Legemiddel {
    protected String navn;
    protected int pris;
    protected double virkestoff;
    private static int teller = 1;
    private int id = 1;

    public Legemiddel(String navn, int pris, double virkestoff) {
        this.navn = navn;
        this.pris = pris;
        this.virkestoff = virkestoff;
        id = teller++;
    }

    public int hentId() {
        return id;
    }

    public String hentNavn() {
        return navn;
    }

    public int hentPris() {
        return pris;
    }

    public double hentVirkestoff() {
        return virkestoff;
    }

    public void settNyPris(int nyPris) {
        pris = nyPris;
    }

    @Override // Overskriver klassen Object sin toString metode
    public String toString() {
        // String.format lar meg lage en lengre string uten å ha så mange "," eller "+" mellom alle variabler
        // %s for generell input, %d for heltall, %.xf for desimaltall med x antall 0er
        return String.format("Legemiddelet %s er id %d. Det koster %d og har et virkestoff paa %.2f mikrogram", navn, id, pris, virkestoff);
    }
}