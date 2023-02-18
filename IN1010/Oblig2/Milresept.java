public class Milresept extends Hvitresept {
    public Milresept(Legemiddel refLegemiddel, Lege refLege, int pasientId) {
        super(refLegemiddel, refLege, pasientId, 0);
        this.reit = 3;
        this.gjeldendepris = 0;
    }
}