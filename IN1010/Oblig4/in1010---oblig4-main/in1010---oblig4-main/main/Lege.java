// D)
public class Lege implements Comparable<Lege>{
    protected String legenavn;
    IndeksertListe<Resept> utskrevneResepter = new IndeksertListe<>(); 

    Lege(String legenavn){
        this.legenavn = legenavn;
    }

    public String hentNavn(){
        return legenavn;
    }

    public IndeksertListe<Resept> hentUtskrevneResepter() {
        return utskrevneResepter;
    }

    public HvitResept skrivHvitResept(Legemiddel legemiddel, Pasient pasient, int reit) throws UlovligUtskrift {
        HvitResept hvitResept = new HvitResept(legemiddel, this, pasient, reit);
        utskrevneResepter.leggTil(hvitResept);
        return hvitResept;
    }
    public MilResept skrivMilResept(Legemiddel legemiddel, Pasient pasient) throws UlovligUtskrift {
        MilResept milResept = new MilResept(legemiddel, this, pasient);
        utskrevneResepter.leggTil(milResept);
        return milResept;
    }
    public PResept skrivPResept(Legemiddel legemiddel, Pasient pasient, int reit) throws UlovligUtskrift {
        PResept pResept = new PResept(legemiddel, this, pasient, reit);
        utskrevneResepter.leggTil(pResept);
        return pResept;
    }
    public BlaaResept skrivBlaaResept(Legemiddel legemiddel, Pasient pasient, int reit) throws UlovligUtskrift {
        BlaaResept blaaResept = new BlaaResept(legemiddel, this, pasient, reit);
        utskrevneResepter.leggTil(blaaResept);
        return blaaResept;
    }

    //midlertidig for TestLege: kommenter ut indeksert objekt og metode for at compareto skal fungere for TestLege.
    @Override
    public int compareTo(Lege annenLege) {
        return this.legenavn.toLowerCase().compareTo(annenLege.hentNavn().toLowerCase());
    }   

    public String toString(){
        return "Legenavn: " + legenavn;
    }
}
