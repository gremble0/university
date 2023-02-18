public class PResept extends HvitResept {
    PResept(Legemiddel legemiddel, Lege lege, Pasient p, int reit) {
        super(legemiddel, lege, p, reit);
    }

    @Override
    public int prisAaBetale(int pris){
        if (pris - 108 <= 0) 
            return 0;
        return pris - 108; 
    }

    @Override
    public String toString() {
        return super.toString() + "\nFarge: Hvitt" + "\n108 NOK rabatt";
    }
}