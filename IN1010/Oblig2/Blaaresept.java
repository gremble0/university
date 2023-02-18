public class Blaaresept extends Resept {
    public Blaaresept(Legemiddel refLegemiddel, Lege refLege, int pasientId, int reit) {
        super(refLegemiddel, refLege, pasientId, reit);
        this.gjeldendepris = (int) (this.gjeldendepris * 0.25);
    }

    @Override // definerer Resept sin farge metode
    public String farge() {
        return "blaa";
    }

    @Override
    public int prisAaBetale() {
        return gjeldendepris;
    }
}
