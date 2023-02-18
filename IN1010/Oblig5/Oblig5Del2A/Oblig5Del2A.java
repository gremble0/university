import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;

public class Oblig5Del2A {
    public static void main(String[] args) {
        try {
            Monitor2 monitor = new Monitor2();
            lesFraMappe(args[0], monitor);

            int hoyestAnt = 0;
            String hoyestStreng = "";
            HashMap<String, Subsekvens> sumMap = new HashMap<String, Subsekvens>();
            for (HashMap<String, Subsekvens> map : monitor.hentListe()) {
                sumMap = monitor.flettToMaps(map, sumMap);
                for (HashMap.Entry<String, Subsekvens> entry : map.entrySet()) {
                    if (entry.getValue().hentAntall() > hoyestAnt) {
                        hoyestAnt = entry.getValue().hentAntall();
                        hoyestStreng = entry.getKey() + " har flest forekomster med " + hoyestAnt;
                    }
                }
            }
            System.out.println(hoyestStreng);
        } catch (IndexOutOfBoundsException e) {
            System.out.println("Vennligst oppgi en mappe");
            e.printStackTrace();
        }
    }
    
    public static void lesFraMappe(String mappeNavn, Monitor2 monitor) {
        try {
            File fil = new File(mappeNavn + "/metadata.csv");
            Scanner leser = new Scanner(fil);
            ArrayList<Thread> traader = new ArrayList<>();

            while (leser.hasNextLine()) {
                String linje = leser.nextLine();
                if (linje.contains("amino_acid")) {continue;} // ikke lag en traad linjer av filer hvis det har denne strengen
                
                Runnable leseTraad;
                if (linje.substring(linje.length() - 4).equals("True") || linje.substring(linje.length() - 5).equals("False")) {
                    leseTraad = new LeseTrad(mappeNavn + "/" + linje.split(",")[0], monitor);
                } else {
                    leseTraad = new LeseTrad(mappeNavn + "/" + linje, monitor);
                }

                Thread traad = new Thread(leseTraad);
                traad.start();
                traader.add(traad);
            }
            
            for (Thread traad : traader) {
                traad.join(); // venter paa at alle traadene skal bli ferdig
            }

            leser.close();

        } catch (FileNotFoundException e) {
            System.out.println("Fant ikke filen.");
            e.printStackTrace();
        } catch (InterruptedException e) {
            System.out.println("Traad avbrutt");
            e.printStackTrace();
        }
    }
}