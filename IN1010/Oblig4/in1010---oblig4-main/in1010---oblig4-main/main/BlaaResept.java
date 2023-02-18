public class BlaaResept extends Resept{
    BlaaResept(Legemiddel legemiddel, Lege lege, Pasient p, int reit) {
        super(legemiddel, lege, p, reit);
    }

    @Override
    public String farge(){
        return "Blaa";
    }

    @Override
    public int prisAaBetale(int pris){
        return (int) Math.ceil(pris * 0.25);
    }

    @Override
    public String toString() {
        return super.toString() + "\nFarge: Blaa" + "\n75% rabatt";
    }
}
