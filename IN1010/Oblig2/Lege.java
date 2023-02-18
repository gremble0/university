public class Lege {
    String navn;
    public Lege(String navn) {
        this.navn = navn;
    }

    public String hentNavn() {
        return navn;
    }

    @Override
    public String toString() {
        return "Legen heter " + navn + ".";
    }
}