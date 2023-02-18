public class HvitResept extends Resept {
    HvitResept(Legemiddel legemiddel, Lege lege, Pasient p, int reit) {
        super(legemiddel, lege, p,reit);
    }

    @Override
    public String farge(){
        return "Hvitt";
    }

    @Override
    public int prisAaBetale(int pris){
        return pris;
    }

    @Override
    public String toString() {
        return super.toString() + "\nFarge: Hvitt";
    }
}
