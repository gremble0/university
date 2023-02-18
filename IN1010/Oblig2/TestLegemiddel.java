public class TestLegemiddel {
    public static void main(String[] args) {
        // Lager 3 instanser av de ulike subklassene
        Vanedannende vaneLegemiddel = new Vanedannende("Hasj", 500, 10.5, 20);
        Narkotisk narkLegemiddel = new Narkotisk("Morfin", 200, 50.0, 75);
        Vanlig vanlLegemiddel = new Vanlig("Nesespray", 100, 2.0);

        // Ønsket tilbakemelding i konsoll er alle strengene etterfulgt av "true",
        // f.eks: "Navn: true", "Id: true" etc.
        System.out.println("--- VANEDANNENDE LEGEMIDDEL ---" +
            "\nNavn: " + testLegemiddelNavn(vaneLegemiddel, "Hasj") + 
            "\nId: " + testLegemiddelId(vaneLegemiddel, 1) + 
            "\nStyrke: " + testLegemiddelStyrke(vaneLegemiddel, 20) + 
            "\nVirkestoff: " + testLegemiddelVirkestoff(vaneLegemiddel, 10.5) +
            "\nPris: " + testLegemiddelPris(vaneLegemiddel, 500) +
            "\n" + vaneLegemiddel
        );

        System.out.println("\n--- NARKOTISK LEGEMIDDEL ---" +
            "\nNavn: " + testLegemiddelNavn(narkLegemiddel, "Morfin") + 
            "\nId: " + testLegemiddelId(narkLegemiddel, 2) + 
            "\nStyrke: " + testLegemiddelStyrke(narkLegemiddel, 75) + 
            "\nVirkestoff: " + testLegemiddelVirkestoff(narkLegemiddel, 50.0) +
            "\nPris: " + testLegemiddelPris(narkLegemiddel, 200) +
            "\n" + narkLegemiddel
        );

        System.out.println("\n--- VANLIG LEGEMIDDEL ---" +
            "\nNavn: " + testLegemiddelNavn(vanlLegemiddel, "Nesespray") + 
            "\nId: " + testLegemiddelId(vanlLegemiddel, 3) + 
            "\nVirkestoff: " + testLegemiddelVirkestoff(vanlLegemiddel, 2.0) +
            "\nPris: " + testLegemiddelPris(vanlLegemiddel, 100) + 
            "\n" + vanlLegemiddel
        );
    }

    public static boolean testLegemiddelId(Legemiddel legemiddel, int forventetLegemiddelId) {
        return legemiddel.hentId() == forventetLegemiddelId;
    }

    public static boolean testLegemiddelNavn(Legemiddel legemiddel, String forventetLegemiddelNavn) {
        return legemiddel.hentNavn() == forventetLegemiddelNavn;
    }

    public static boolean testLegemiddelPris(Legemiddel legemiddel, int forventetLegemiddelPris) {
        return legemiddel.hentPris() == forventetLegemiddelPris;
    }

    public static boolean testLegemiddelVirkestoff(Legemiddel legemiddel, double forventetLegemiddelVirkestoff) {
        return legemiddel.hentVirkestoff() == forventetLegemiddelVirkestoff;
    }

    public static boolean testLegemiddelStyrke(Legemiddel legemiddel, int forventetLegemiddelStyrke) {
        // sjekker om legemiddelet gitt som parameter er narkotisk eller vanedannende, caster det til tilsvarende type
        // og returnerer om styrken samsvarer med gitt argument
        if (legemiddel instanceof Narkotisk) {
            Narkotisk narkMiddel = (Narkotisk) legemiddel;
            return narkMiddel.hentNarkotiskStyrke() == forventetLegemiddelStyrke;
        } else if (legemiddel instanceof Vanedannende) {
            Vanedannende vaneMiddel = (Vanedannende) legemiddel;
            return vaneMiddel.hentVanedannendeStyrke() == forventetLegemiddelStyrke;
        }
        return false;
    }

}