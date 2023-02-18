abstract public class Resept {
    protected Legemiddel refLegemiddel;
    protected Lege refLege;
    protected int pasientId;
    protected int reit;
    protected int gjeldendepris;
    private static int teller = 1;
    private int id = 1;

    public Resept(Legemiddel refLegemiddel, Lege refLege, int pasientId, int reit) {
        this.refLegemiddel = refLegemiddel;
        this.refLege = refLege;
        this.pasientId = pasientId;
        this.reit = reit;
        // definerer egen pris for denne klassen for å kunne lage rabatter på resepter uten å endre på prisen til legemiddelet
        this.gjeldendepris = refLegemiddel.hentPris(); 
        id = teller++;
    }
    
    public int hentId() {
        return id;
    }

    public Legemiddel hentLegemiddel() {
        return refLegemiddel;
    }

    public Lege hentLege() {
        return refLege;
    }

    public int hentPasientId() {
        return pasientId;
    }

    public int hentReit() {
        return reit;
    }

    public boolean bruk() {
        if (reit > 0) {
            reit--;
            return true;
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        return String.format("Resept med id %d er for %s. Resepten er til pasient %d og var gitt av legen %s. Den har %d antall bruk igjen. Pasienten maa betale %d for resepten.",
        id, refLegemiddel.hentNavn(), pasientId, refLege.hentNavn(), reit, gjeldendepris);
    }

    abstract public String farge();
    abstract public int prisAaBetale();
}
