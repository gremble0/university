import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;

public class Oblig5Hele {
    // hvor mange traader som jobber med fletting, og lesing (antTraader*2 vil 
    // vaere hvor mange traader som kjoeres samtidig i dette programmet)
    private static final int antTraader = 8;
    public static void main(String[] args) {
        Monitor2 syk = new Monitor2();
        Monitor2 frisk = new Monitor2();
        Monitor2[] monitorer = {syk, frisk};

        try {
            lesFraMappe(args[0], monitorer);

            Thread traader[] = new Thread[antTraader*2];
            for (int i = 0; i < antTraader; i++) {
                traader[i] = new Thread(new FletteTrad(syk));
                traader[i].start();
            }
            for (int i = 8; i < antTraader*2; i++) {
                traader[i] = new Thread(new FletteTrad(frisk));
                traader[i].start();
            }

            // venter paa at alle traadene skal bli ferdig
            for (Thread traad : traader) {
                traad.join();
            }

            // hvis filene i mappen ikke har sannhetsverdier
            if (syk.hentStoerrelse() == 0) {
                int hoyestAnt = 0;
                String hoyestStreng = "";
                HashMap<String, Subsekvens> map = frisk.hentFraIndex(0);
                for (HashMap.Entry<String, Subsekvens> entry : map.entrySet()) {
                    if (entry.getValue().hentAntall() > hoyestAnt) {
                        hoyestAnt = entry.getValue().hentAntall();
                        hoyestStreng = entry.getKey() + " har flest forekomster med " + hoyestAnt;
                    }
                }
                System.out.println(hoyestStreng);
                System.exit(0); // avslutt 
            }

            HashMap<String, Subsekvens> sykMap = syk.hentFraIndex(0); // invariant: stoerrelse er naa = 1
            HashMap<String, Subsekvens> friskMap = frisk.hentFraIndex(0);
            for (HashMap.Entry<String, Subsekvens> entry : sykMap.entrySet()) {
                try {
                    if (entry.getValue().hentAntall() >= friskMap.get(entry.getKey()).hentAntall() + 7) {
                        System.out.println(String.format("%s har vesentlig fler forekomster (%d) fra syk til frisk", 
                            entry.getKey(), 
                            entry.getValue().hentAntall() - friskMap.get(entry.getKey()).hentAntall()
                            )
                        );
                    }
                } catch (NullPointerException e) {
                    continue;
                }
            }

            System.exit(0); // avslutt program naar main traad har kommet hit

        } catch (IndexOutOfBoundsException e) {
            System.out.println("Vennligst oppgi en mappe");
            e.printStackTrace();
        } catch (InterruptedException e) {
            System.out.println("Traad avbrutt");
            e.printStackTrace();
        }
    }

    public static void lesFraMappe(String mappeNavn, Monitor2[] monitorer) {
        try {
            File fil = new File(mappeNavn + "/metadata.csv");
            Scanner leser = new Scanner(fil);
            ArrayList<Thread> traader = new ArrayList<>();

            while (leser.hasNextLine()) {
                String linje = leser.nextLine();
                if (linje.contains("amino_acid")) {continue;} // ikke lag en traad for forste linje av filer i data mappen
                
                Runnable leseTraad;
                if (linje.substring(linje.length() - 4).equals("True")) {
                    leseTraad = new LeseTrad(mappeNavn + "/" + linje.split(",")[0], monitorer[0]);
                } else if (linje.substring(linje.length() - 5).equals("False")) {
                    leseTraad = new LeseTrad(mappeNavn + "/" + linje.split(",")[0], monitorer[1]);
                } else {
                    leseTraad = new LeseTrad(mappeNavn + "/" + linje, monitorer[1]);
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