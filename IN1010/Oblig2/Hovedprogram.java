public class Hovedprogram {
    public static void main(String[] args) {
        // gjorde egentlig dette for det meste i TestResepter, men her er en litt utvidet versjon
        Vanedannende vaneLegemiddel = new Vanedannende("Hasj", 500, 10.5, 20);
        Narkotisk narkLegemiddel = new Narkotisk("Morfin", 200, 50.0, 75);
        Vanlig vanlLegemiddel = new Vanlig("Nesespray", 100, 2.0);

        Spesialist jon = new Spesialist("Jon", "AF1202");
        Lege andreas = new Lege("Andreas");
        Lege svein = new Lege("Svein");

        Hvitresept hvitresept = new Hvitresept(vaneLegemiddel, jon, 89, 5);
        Milresept milresept = new Milresept(vaneLegemiddel, andreas, 22);
        Presept presept = new Presept(narkLegemiddel, jon, 22, 5);
        Blaaresept blaaresept = new Blaaresept(vanlLegemiddel, svein, 44, 10);

        // Ønsket tilbakemelding i konsoll er f.eks: "Id: true", "Legemiddel klasse: true" etc.

        //
        // LEGEMIDLER
        //
        System.out.println("--------- LEGEMIDLER ---------");
        
        System.out.println("\n--- VANEDANNENDE LEGEMIDDEL ---" +
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

        //
        // RESEPTER
        //

        System.out.println("\n--------- RESEPTER ---------");

        System.out.println("\n--- HVIT RESEPT --- " +
            "\nId: " + testReseptId(hvitresept, 1) + 
            "\nLegemiddel klasse: " + testReseptLegemiddel(hvitresept, vaneLegemiddel) + 
            "\nLege: " + testReseptLege(hvitresept, jon) + 
            "\nPris: " + testReseptPasientId(hvitresept, 89) +
            "\nReit: " + testReseptReit(hvitresept, 5) +
            "\n" + hvitresept
        );

        System.out.println("\n--- MILITAER RESEPT --- " +
            "\nId: " + testReseptId(milresept, 2) + 
            "\nLegemiddel klasse: " + testReseptLegemiddel(milresept, vaneLegemiddel) + 
            "\nLege: " + testReseptLege(milresept, andreas) + 
            "\nPris: " + testReseptPasientId(milresept, 22) +
            "\nReit: " + testReseptReit(milresept, 3) + // milresept har alltid 3 reit
            "\n" + milresept
        );

        System.out.println("\n--- P-RESEPT --- " +
            "\nId: " + testReseptId(presept, 3) + 
            "\nLegemiddel klasse: " + testReseptLegemiddel(presept, narkLegemiddel) + 
            "\nLege: " + testReseptLege(presept, jon) + 
            "\nPris: " + testReseptPasientId(presept, 22) +
            "\nReit: " + testReseptReit(presept, 5) +
            "\n" + presept
        );

        System.out.println("\n--- BLAA RESEPT --- " +
            "\nId: " + testReseptId(blaaresept, 4) + 
            "\nLegemiddel klasse: " + testReseptLegemiddel(blaaresept, vanlLegemiddel) + 
            "\nLege: " + testReseptLege(blaaresept, svein) + 
            "\nPris: " + testReseptPasientId(blaaresept, 44) +
            "\nReit: " + testReseptReit(blaaresept, 10) +
            "\n" + blaaresept
        );

        //
        // LEGER
        //

        System.out.println("\n--------- LEGER/SPESIALISTER ---------");

        System.out.println("\n --- JON ---" +
            "\nNavn: " + testLegeNavn(jon, "Jon") +
            "\nSpesialist: " + testLegeSpesialist(jon, true) +
            "\nKontrollId: " + testSpesialistKontrollId(jon, "AF1202") + 
            "\n" + jon
        );

        System.out.println("\n --- ANDREAS ---" +
            "\nNavn: " + testLegeNavn(andreas, "Andreas") +
            "\nSpesialist: " + testLegeSpesialist(andreas, false) +
            "\n" + andreas
        );

        System.out.println("\n --- SVEIN ---" +
            "\nNavn: " + testLegeNavn(svein, "Svein") +
            "\nSpesialist: " + testLegeSpesialist(svein, false) +
            "\n" + svein
        );
    }

    //
    // LEGEMIDDEL TESTER
    //
    
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

    //
    // RESEPT TESTER
    //

    public static boolean testReseptId(Resept resept, int forventetReseptId) {
        return resept.hentId() == forventetReseptId;
    }

    public static boolean testReseptLegemiddel(Resept resept, Legemiddel forventetLegemiddel) {
        // Sjekker om klassen av resepten er den forventede klassen
        return resept.hentLegemiddel().getClass() == forventetLegemiddel.getClass();
    }

    public static boolean testReseptLege(Resept resept, Lege forventetLege) {
        return resept.hentLege() == forventetLege;
    }

    public static boolean testReseptPasientId(Resept resept, int forventetPasientId) {
        return resept.hentPasientId() == forventetPasientId;
    }

    public static boolean testReseptReit(Resept resept, int forventetReit) {
        return resept.hentReit() == forventetReit;
    }

    // 
    // LEGE/SPESIALIST TESTER
    //

    public static boolean testLegeNavn(Lege lege, String forventetNavn) {
        return lege.hentNavn().equals(forventetNavn);
    }

    public static boolean testLegeSpesialist(Lege lege, boolean forventetSannhetsverdi) {
        // hvis vi forventer at legen er en spesialist returnerer vi lege instanceof Spesialist
        // hvis ikke returnerer vi det motsatte av den samme sjekken
        if (forventetSannhetsverdi) {
            return lege instanceof Spesialist;
        }
        return !(lege instanceof Spesialist);
    }

    public static boolean testSpesialistKontrollId(Spesialist spesialist, String forventetId) {
        return spesialist.hentKontrollId().equals(forventetId);
    }
}
