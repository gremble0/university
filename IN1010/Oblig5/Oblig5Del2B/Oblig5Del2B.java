import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;

public class Oblig5Del2B {
    private static final int antTraader = 8;
    public static void main(String[] args) {
        Monitor2 monitor = new Monitor2();

        try {
            lesFraMappe(args[0], monitor);

            Thread traader[] = new Thread[antTraader];
            for (int i = 0; i < antTraader; i++) {
                traader[i] = new Thread(new FletteTrad(monitor));
                traader[i].start();
            }

            // venter paa at alle traadene skal bli ferdig
            for (Thread traad : traader) {
                traad.join();
            }

            int hoyestAnt = 0;
            String hoyestStreng = "";
            HashMap<String, Subsekvens> map = monitor.hentFraIndex(0);
            for (HashMap.Entry<String, Subsekvens> entry : map.entrySet()) {
                // vil kun gi et svar hvis det er flere subsekvenser med samme antall
                if (entry.getValue().hentAntall() > hoyestAnt) {
                    hoyestAnt = entry.getValue().hentAntall();
                    hoyestStreng = entry.getKey() + " har flest forekomster med " + hoyestAnt;
                }
            }
            System.out.println(hoyestStreng);

            System.exit(0); // avslutt program naar main traad har kommet hit

        } catch (IndexOutOfBoundsException e) {
            System.out.println("Vennligst oppgi en mappe");
            e.printStackTrace();
        } catch (InterruptedException e) {
            System.out.println("Traad avbrutt");
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
                traad.join(); 
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
