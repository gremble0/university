abstract public class Legemiddel{
    protected String navn;
    protected static int idTeller;
    protected int ID;
    protected int pris;
    protected double virkestoff;

    Legemiddel(String navn, int pris, double virkestoff){
        idTeller++;
        ID = idTeller;
        this.navn = navn;
        this.pris = pris;
        this.virkestoff = virkestoff;
    }

    public int hentId(){
        return ID;
    }

    public String hentNavn(){
        return navn;
    }

    public int hentPris(){
        return pris;
    }

    public double hentVirkestoff(){
        return virkestoff;
    }

    public String toString(){
        return ("Navn: " + navn + "\nLegemiddel ID: " + ID + "\nPris: " + pris+ "\nVirkestoff: " + virkestoff);
    }
}