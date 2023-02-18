public class Presept extends Hvitresept {
    public Presept(Legemiddel refLegemiddel, Lege refLege, int pasientId, int reit) {
        super(refLegemiddel, refLege, pasientId, reit);
        // hvis prisen er over 108 - trekk 108 fra prisen, ellers (prisen er under eller lik 108) gjoer prisen til 0
        if (this.gjeldendepris > 108) {
            this.gjeldendepris -= 108;
        } else {
            this.gjeldendepris = 0;
        }
    }
}
