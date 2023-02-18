import java.util.ArrayList;
import java.util.HashMap;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class SubsekvensBeholder {
    protected ArrayList<HashMap<String, Subsekvens>> liste = new ArrayList<HashMap<String, Subsekvens>>();
    protected static final int subsekLengde = 3; // kan endres

    // setter inn hashmap med String som noekkel og Subsekvens som verdi i liste
    public void settInn(HashMap<String, Subsekvens> map) {
        liste.add(map);
    }

    // returnerer listen
    public ArrayList<HashMap<String, Subsekvens>> hentListe() {
        return liste;
    }

    // henter et tilfeldig hashmap fra listen
    public HashMap<String, Subsekvens> hentTilfeldig() {
        int tilfeldigIndeks = (int)(Math.random() * liste.size());
        return liste.get(tilfeldigIndeks);
    }

    // henter et element paa gitt index
    public HashMap<String, Subsekvens> hentFraIndex(int index) {
        return liste.get(index);
    }

    // fjerner gitt map
    public void fjernMap(HashMap<String, Subsekvens> map) {
        hentListe().remove(map);
    }

    // henter stoerrelsen til listen
    public int hentStoerrelse() {
        return liste.size();
    }

    // leser data fra fil og returnerer et ferdigbygget hashmap
    public static HashMap<String, Subsekvens> lesFraFil(String filnavn) {
        HashMap<String, Subsekvens> map = new HashMap<String, Subsekvens>();

        try {
            File fil = new File(filnavn);
            Scanner leser = new Scanner(fil);
            while (leser.hasNextLine()) {
                String linje = leser.nextLine();
                if (linje.length() < subsekLengde) {
                    System.exit(0); // avslutter hele programmet, evt return null hvis vi bare vil avslutte metode
                }
                for (int i = 0; i <= linje.length() - subsekLengde; i++) {
                    Subsekvens subs = new Subsekvens(linje.substring(i, i + subsekLengde), 1);
                    map.put(linje.substring(i, i + subsekLengde), subs);
                }
            }
            leser.close();
        } catch (FileNotFoundException e) {
            System.out.println("Fant ikke filen");
            e.printStackTrace();
        }
        return map;
    }

    public HashMap<String, Subsekvens> flettToMaps(HashMap<String, Subsekvens> map1, HashMap<String, Subsekvens> map2) {
        // tar verdier fra map2 og legger dem inn i map1
        for (HashMap.Entry<String, Subsekvens> entry : map2.entrySet()) {
            if (!(map1.get(entry.getKey()) == null)) { // hvis map1 har noekkelen til gjeldende element fra map2...
                // legger inn det som er der fra foer i map1 + det som er der i map2 inn i noekkelen vi itererer gjennom
                int nyttAntall = map1.get(entry.getKey()).hentAntall() + entry.getValue().hentAntall();
                map1.get(entry.getKey()).endreAntall(nyttAntall);
            } else { // hvis map1 ikke har noekkelen...
                map1.put(entry.getKey(), entry.getValue());
            }
        }
        return map1;
    }
}