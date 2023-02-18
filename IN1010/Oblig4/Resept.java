abstract public class Resept {
    protected Legemiddel legemiddel;
    protected Lege utskrivendeLege;
    //protected int pasientId;
    protected int reit;
    protected static int idTeller;
    protected int ID;
    protected Pasient pasient;

    Resept(Legemiddel legemiddel, Lege utskrivendeLege, Pasient p, int reit){
        idTeller++;
        ID = idTeller;
        this.legemiddel = legemiddel;
        this.utskrivendeLege = utskrivendeLege;
        this.reit = reit;
        pasient = p;
    }

    public int hentID(){
        return ID;
    }

    public Legemiddel hentLegemiddel(){
        return legemiddel;   
    }

    public Lege hentLege(){
        return utskrivendeLege;
    }

    public int hentPasientId(){
        return pasient.hentId();      
    }

    public int hentReit(){
        return reit;      
    }


    public boolean bruk(){
        if (reit == 0)
            return false;
        reit--;
        return true;      
    }

    abstract public String farge();

    abstract public int prisAaBetale(int pris);

    public String toString(){
        return ("Legemiddel: " + legemiddel + "\nUtskrivendeLege: " + utskrivendeLege + "\nPasient ID: " + hentPasientId() + "\nReit: " + reit);
    }
}
