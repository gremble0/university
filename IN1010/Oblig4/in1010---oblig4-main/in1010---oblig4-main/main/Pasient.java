public class Pasient {
    String navn, fodselsnummer;
    int ID;
    static int antPasient = 0;
    Stabel<Resept> reseptStabel = new Stabel<>();
    
    public Pasient(String n, String fnr){
        navn = n;
        fodselsnummer = fnr;
        ID = antPasient;
        antPasient++;
    }

    public void leggTil(Resept resept) {
        reseptStabel.leggTil(resept);
    }

    public String hentNavn() {return navn;}
    public String hentFoedselsnr() {return fodselsnummer;}
    public int hentId() {return ID;}
    public Stabel<Resept> hentStabel() {return reseptStabel;}

    public String toString(){
        return "Pasientnavn: " + navn + "\nFodselsnummer: " + fodselsnummer + "\nID: " + ID;
    }
      
}
