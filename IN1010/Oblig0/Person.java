public class Person {
    Bil3 personBil;
    public Person(Bil3 bil) {
        personBil = bil;
    }

    public void skrivUtBilNr() {
        System.out.println("Denne personen eier bilen ved nummer: " + personBil.hentNummer());
    }
}
