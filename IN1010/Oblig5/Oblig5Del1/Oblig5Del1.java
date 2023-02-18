import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.HashMap;

class Oblig5Del1 {
    public static void main(String[] args) {
        SubsekvensBeholder beholder = new SubsekvensBeholder();
        lesFraMappe(args[0], beholder);
        
        int hoyestAnt = 0;
        String hoyestStreng = "";
        HashMap<String, Subsekvens> sumMap = new HashMap<String, Subsekvens>();
        for (HashMap<String, Subsekvens> map : beholder.hentListe()) {
            sumMap = beholder.flettToMaps(map, sumMap);
            for (HashMap.Entry<String, Subsekvens> entry : map.entrySet()) {
                if (entry.getValue().hentAntall() > hoyestAnt) {
                    hoyestAnt = entry.getValue().hentAntall();
                    hoyestStreng = entry.getKey() + " har flest forekomster med " + hoyestAnt;
                }
            }
        }
        System.out.println(hoyestStreng);
    }

    // gaar inn i mappe aapner metadata filen og leser av alle filnavnene som skal leses fra
    // kaller saa paa SubsekvensBeholder sin lesFraFil for aa lese alle filene.
    // forventer at det er en mappe med en metadata.csv fil i samme mappe som denne java filen
    public static void lesFraMappe(String mappeNavn, SubsekvensBeholder beholder) {
        try {
            File fil = new File(mappeNavn + "/metadata.csv");
            Scanner leser = new Scanner(fil);
            // while (leser.hasNextLine()) {
            //     String linje = leser.nextLine();
            //     beholder.settInn(beholder.lesFraFil(mappeNavn + "/" + linje));
            // }

            while (leser.hasNextLine()) {
                String linje = leser.nextLine();
                if (linje.contains("amino_acid")) {continue;} // ikke lag en traad linjer av filer hvis det har denne strengen
                
                if (linje.substring(linje.length() - 4).equals("True") || linje.substring(linje.length() - 5).equals("False")) {
                    beholder.settInn(SubsekvensBeholder.lesFraFil(mappeNavn + "/" + linje.split(",")[0]));
                } else {
                    beholder.settInn(SubsekvensBeholder.lesFraFil(mappeNavn + "/" + linje));
                }
            }

            leser.close();
        } catch (FileNotFoundException e) {
            System.out.println("Fant ikke filen.");
            e.printStackTrace();
        }
    }
}