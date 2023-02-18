import java.util.FormatFlagsConversionMismatchException;

import javax.swing.plaf.synth.SynthSeparatorUI;

public class TestResepter {
    public static void main(String[] args) {
        Vanedannende vaneLegemiddel = new Vanedannende("Hasj", 500, 10.5, 20);
        Narkotisk narkLegemiddel = new Narkotisk("Morfin", 200, 50.0, 75);
        Vanlig vanlLegemiddel = new Vanlig("Nesespray", 100, 2.0);

        Lege jon = new Lege("Jon");
        Lege andreas = new Lege("Andreas");
        Lege svein = new Lege("Svein");

        Hvitresept hvitresept = new Hvitresept(vaneLegemiddel, jon, 89, 5);
        Milresept milresept = new Milresept(vaneLegemiddel, andreas, 22);
        Presept presept = new Presept(narkLegemiddel, jon, 22, 5);
        Blaaresept blaaresept = new Blaaresept(vanlLegemiddel, svein, 44, 10);
        
        // Ønsket tilbakemelding i konsoll er f.eks: "Id: true", "Legemiddel klasse: true" etc.
        System.out.println("--- HVIT RESEPT --- " +
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
    }

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
}
