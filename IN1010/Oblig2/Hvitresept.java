public class Hvitresept extends Resept {
    public Hvitresept(Legemiddel refLegemiddel, Lege refLege, int pasientId, int reit) {
        super(refLegemiddel, refLege, pasientId, reit);
    }

    @Override // definerer Resept sin farge metode
    public String farge() {
        return "hvit";
    }

    @Override
    public int prisAaBetale() {
        return gjeldendepris;
    }
}
