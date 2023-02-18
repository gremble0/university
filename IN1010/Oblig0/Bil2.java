public class Bil2 {
    String bilNummer;
    public Bil2(String bilNr) {
        bilNummer = bilNr;
    }

    public void SkrivUt() {
        System.out.println("Jeg er bilen '" + bilNummer + "'");
    }
}
