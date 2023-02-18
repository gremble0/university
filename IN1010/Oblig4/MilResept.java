public class MilResept extends HvitResept {
    MilResept(Legemiddel legemiddel, Lege lege, Pasient p) {
        super(legemiddel, lege, p, 3);
    }

    @Override
    public int prisAaBetale(int pris){
        return 0;
    }

    @Override
    public String toString() {
        return super.toString() + "\nFarge: Hvitt" + "\n100% rabatt";
    }
}
