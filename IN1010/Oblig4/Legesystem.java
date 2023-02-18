import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.File;
import java.util.Scanner;

public class Legesystem {
    IndeksertListe<Pasient> pasienter = new IndeksertListe<>();
    IndeksertListe<Legemiddel> legemidler = new IndeksertListe<>();
    Prioritetskoe<Lege> leger = new Prioritetskoe<>();
    int vanedannendeBrukt = 0;
    int narkotiskBrukt = 0;

    public void lesFraFil(String filnavn) {
        try {
            File f = new File(filnavn);
            Scanner leser = new Scanner(f);
            String gjeldendeType = "Pasienter"; // Forventer Pasienter som start av fil
            String linje = leser.nextLine(); // .trim() fordi noen filer har " " paa slutten av linjen
            while (leser.hasNextLine()) {
                linje = leser.nextLine();
                String[] linje_split_mellomrom = linje.trim().split(" ");
                if (linje_split_mellomrom[0].equals("#")) {
                    gjeldendeType = linje_split_mellomrom[1];
                }
                leggTilLinje(gjeldendeType, linje, leser);
            }
            leser.close();
        } catch (FileNotFoundException e) {
            System.out.println("Fant ikke filen.");
            e.printStackTrace();
        }
    }

    public void leggTilLinje(String type, String linje, Scanner leser) { // maa sende med linje for aa ha tilgang til
                                                                         // forrige linje
        String[] linje_split_komma = linje.trim().split(",");
        String[] linje_split_mellomrom = linje.trim().split(" ");

        if (linje_split_mellomrom[0].equals("#")) {
            linje = leser.nextLine();
            linje_split_komma = linje.trim().split(",");
        }
        if (type.equals("Pasienter")) {
            pasienter.leggTil(new Pasient(linje_split_komma[0], linje_split_komma[1])); // bruker Lenkeliste sin
                                                                                        // leggTil()
        } else if (type.equals("Legemidler")) {
            if (linje_split_komma[1].equals("narkotisk")) {
                legemidler.leggTil(new Narkotisk(linje_split_komma[0], Integer.parseInt(linje_split_komma[2]),
                        Double.parseDouble(linje_split_komma[3]),
                        Integer.parseInt(linje_split_komma[4])));
            } else if (linje_split_komma[1].equals("vanedannende")) {
                legemidler.leggTil(new Vanedannende(linje_split_komma[0], Integer.parseInt(linje_split_komma[2]),
                        Double.parseDouble(linje_split_komma[3]), Integer.parseInt(linje_split_komma[4])));
            } else if (linje_split_komma[1].equals("vanlig")) {
                legemidler.leggTil(new Vanlig(linje_split_komma[0], Integer.parseInt(linje_split_komma[2]),
                        Double.parseDouble(linje_split_komma[3])));
            }
        } else if (type.equals("Leger")) {
            if (Integer.parseInt(linje_split_komma[1]) != 0) {
                leger.leggTil(new Spesialist(linje_split_komma[0], linje_split_komma[1]));
            } else {
                leger.leggTil(new Lege(linje_split_komma[0]));
            }
        } else if (type.equals("Resepter")) {
            int legePos = finnLegePos(linje_split_komma[1]);
            Lege lege = leger.hent(legePos);
            Legemiddel legemiddel = legemidler.hent(Integer.parseInt(linje_split_komma[0]) - 1);
            Pasient pasient = pasienter.hent(Integer.parseInt(linje_split_komma[2]));

            if (linje_split_komma[3].equals("militaer")) {
                Resept resept = new MilResept(legemiddel, lege, pasient);
                pasient.leggTil(resept);
            } else if (linje_split_komma[3].equals("hvit")) {
                Resept resept = new HvitResept(legemiddel, lege, pasient, Integer.parseInt(linje_split_komma[4]));
                pasient.leggTil(resept);
            } else if (linje_split_komma[3].equals("p")) {
                Resept resept = new PResept(legemiddel, lege, pasient, Integer.parseInt(linje_split_komma[4]));
                pasient.leggTil(resept);
            } else if (linje_split_komma[3].equals("blaa")) {
                Resept resept = new BlaaResept(legemiddel, lege, pasient, Integer.parseInt(linje_split_komma[4]));
                pasient.leggTil(resept);
            }
        }
    }

    public int finnLegePos(String navn) {
        int teller = 0;
        for (Lege lege : leger) {
            if (lege.hentNavn().equals(navn)) {
                return teller;
            }
            teller++;
        }
        return -1;
    }

    public void mainLoop() {
        Scanner sc = null;
        String brukerInput = "";

        sc = new Scanner(System.in);

        System.out.println(
                "\nHer har vi noen valgmuligheter:\n" +
                        "Trykk '1' for aa skrive ut alle elementer i legesystemet.\n" +
                        "Trykk '2' for aa lage og legge til nye elementer i systemet.\n" +
                        "Trykk '3' for aa bruke en resept til en pasient.\n" +
                        "Trykk '4' for aa aapne undermeny for vis av statistikk.\n" +
                        "Trykk '5' for aa skrive all data til fil.\n" +
                        "Trykk 'q' for aa avslutte programmet.\n");

        brukerInput = sc.nextLine();
        if (brukerInput.equals("1")) {
            skrivUtAlleElementer(); // E3
        } else if (brukerInput.equals("2")) {
            leggTilElementer(); // E4
        } else if (brukerInput.equals("3")) {
            brukResept(); // E5
        } else if (brukerInput.equals("4")) {
            visStatistikk();
        } else if (brukerInput.equals("5")) {
            skrivTilFil(); // E7
        } else if (brukerInput.equals("q")) {
            System.exit(0);
        } else {
            System.out.println("Uglydig input");
            System.exit(0);
        }
        sc.close();
    }

    public void skrivUtAlleElementer() {
        System.out.println("- - - - - - - - - - - - - - - - - - - - ");
        System.out.println("\nPasienter:");
        for (Pasient p : pasienter) {
            System.out.println(p.hentId() + ": " + p.hentNavn() + "(fnr " + p.hentFoedselsnr() + ")");
        }
        System.out.println("\n- - - - - - - - - - - - - - - - - - - - ");
        System.out.println("\nLegemidler:");
        for (Legemiddel l : legemidler) {
            System.out.println(l + "\n");
        }
        System.out.println("\n- - - - - - - - - - - - - - - - - - - - ");
        System.out.println("\nLeger:");
        for (Lege l : leger) {
            System.out.println(l);
        }
        System.out.println("\n- - - - - - - - - - - - - - - - - - - - ");
        System.out.println("\nResepter:");
        for (Pasient p : pasienter) {
            for (Resept r : p.hentStabel()) {
                String type = "";
                String res = "";
                int reit = 0;
                if (r instanceof MilResept) {
                    type = "militaer";
                } else if (r instanceof BlaaResept) {
                    type = "blaa";
                    reit = r.hentReit();
                } else {
                    type = "hvit";
                    reit = r.hentReit();
                }

                res += String.format("%s,%s,%s,%s",
                        r.hentLegemiddel().hentId(),
                        r.hentLege().hentNavn(),
                        p.hentId(),
                        type);

                if (reit > 0) {
                    res += ("," + r.hentReit());
                }
                System.out.println(res);
            }
        }
        mainLoop();
    }

    @SuppressWarnings("resource")
    public void leggTilElementer() {
        Scanner sc = null;
        String brukerInput = "";

        sc = new Scanner(System.in);

        System.out.println(
                "-LEGGE TIL-\n" +
                        "Her har vi noen valgmuligheter:\n" +
                        "Trykk '1' for aa legge til en pasient.\n" +
                        "Trykk '2' for aa legge til ett legemiddel.\n" +
                        "Trykk '3' for aa legge til en lege.\n" +
                        "Trykk '4' for aa legge til en resept.\n" +
                        "Trykk 'q' for aa avslutte programmet.\n");

        brukerInput = sc.nextLine();
        if (brukerInput.equals("1")) {
            leggTilPasient();
        } else if (brukerInput.equals("2")) {
            leggTilLegemiddel();
        } else if (brukerInput.equals("3")) {
            leggTilLege();
        } else if (brukerInput.equals("4")) {
            leggTilResept();
        } else if (brukerInput.equals("q")) {
            System.exit(0);
        }

    }

    @SuppressWarnings("resource")
    public void leggTilPasient() {
        System.out.println("legger til pasient test");

        Scanner sc = new Scanner(System.in);
        String brukerInput = "";
        String navn;
        String fnr = "";
        Pasient pasient = null;

        System.out.println("Skriv inn navnet paa pasienten: ");
        navn = sc.nextLine().trim();

        System.out.println("Skriv inn fodselsnummer (11 siffer): ");
        brukerInput = sc.nextLine().trim();
        if (brukerInput.length() != 11) {
            System.out.println("Fodselsnummeret er ugyldig: " + brukerInput);
            return;
        }
        try {
            Double.valueOf(brukerInput);
            fnr = brukerInput;
        } catch (Exception e) {
            System.out.println("Fodselsnummeret er ugyldig: " + brukerInput);
            return;
        }

        pasient = new Pasient(navn, fnr);
        pasienter.leggTil(pasient);
        System.out.println("Suksess!");

        // sender tilbake til hovedmenyen
        mainLoop();

    }

    // ferdig
    @SuppressWarnings("resource")
    public void leggTilLegemiddel() {
        System.out.println("legger til legemiddel test");

        Scanner sc = new Scanner(System.in);
        String brukerInput = "";
        String navn;
        int pris;
        double virkestoff;
        int styrke;
        Legemiddel legemiddel = null;

        System.out.println("Skriv inn navnet paa legemiddelet: ");
        navn = sc.nextLine().trim();

        System.out.println("Skriv inn prisen paa legemiddelet: ");
        brukerInput = sc.nextLine().trim();
        try {
            // int-sjekk
            pris = Integer.valueOf(brukerInput);
        } catch (Exception e) {
            System.out.println("Prisen er ugyldig: " + brukerInput);
            return;
        }

        System.out.println("Skriv inn virkestoff (double): ");
        brukerInput = sc.nextLine().trim();
        try {
            // double-sjekk
            virkestoff = Double.valueOf(brukerInput);
        } catch (Exception e) {
            System.out.println("Virkestoffet er ugyldig: " + brukerInput);
            return;
        }

        System.out.println(
                "Velg type: :\n" +
                        "Trykk '1' for <vanlig>\n" +
                        "Trykk '2' for <narkotisk>\n" +
                        "Trykk '3' for for <vanedannende>\n");

        String svar = sc.nextLine().trim();
        if (svar.equals("1")) {
            legemiddel = new Vanlig(navn, pris, virkestoff);
        } else if (svar.equals("2")) {
            System.out.println("Skriv inn styrke for narkotisk legemiddel: ");
            brukerInput = sc.nextLine().trim();
            try {
                // int-sjekk
                styrke = Integer.valueOf(brukerInput);
            } catch (Exception e) {
                System.out.println("Styrken er ugyldig: " + brukerInput);
                return;
            }
            legemiddel = new Narkotisk(navn, pris, virkestoff, styrke);
        } else if (svar.equals("3")) {
            System.out.println("Skriv inn styrke for vanedannende legemiddel: ");
            brukerInput = sc.nextLine().trim();
            try {
                // int-sjekk
                styrke = Integer.valueOf(brukerInput);
            } catch (Exception e) {
                System.out.println("Styrken er ugyldig: " + brukerInput);
                return;
            }
            legemiddel = new Vanedannende(navn, pris, virkestoff, styrke);
        } else {
            System.out.println("Feil input, proev igjen\n");
            leggTilLegemiddel();
        }

        legemidler.leggTil(legemiddel);
        System.out.println("Suksess!");

        // sender tilbake til hovedmenyen
        mainLoop();

    }

    // ferdig
    @SuppressWarnings("resource")
    public void leggTilLege() {
        System.out.println("legger til lege test");

        Scanner sc = new Scanner(System.in);
        int brukerInput;
        String legeNavn;
        String id = ""; // kontroll id
        System.out.println("Skriv inn legen sitt navn: ");
        legeNavn = sc.nextLine().trim();
        Lege lege = null;
        System.out.println(
                "Her har vi noen valgmuligheter:\n" +
                        "Trykk '1' for <ja>\n" +
                        "Trykk '2' for <nei>\n" +
                        "Er legen spesialist?(ja/nei)\n");
        int svar = sc.nextInt();
        if (svar == 1) {
            System.out.println("KontrollID: ");
            if (!(sc.hasNextInt())) {
                System.out.println("Uglydig input");
                System.exit(0);
            }
            brukerInput = sc.nextInt();

            lege = new Spesialist(legeNavn, Integer.toString(brukerInput));
        } else if (svar == 2) {
            lege = new Lege(legeNavn);
        } else {
            System.out.println("Feil input, proev igjen\n");
            leggTilLege();
        }
        leger.leggTil(lege);
        System.out.println("Suksess!");

        // sender tilbake til hovedmenyen
        mainLoop();
    }

    // usikker pa denne
    @SuppressWarnings("resource")
    public void leggTilResept() {
        System.out.println("legger til resept test");

        Scanner sc = new Scanner(System.in);
        int brukerInput;
        int id;
        int reit = 0;
        Pasient pasient = null;
        Legemiddel legemiddel = null;
        Lege lege = null;
        String typeResept = "";
        System.out.println(
                "Her har vi noen valgmuligheter:\n" +
                        "Trykk '1' for <hvit>\n" +
                        "Trykk '2' for <militaer>\n" +
                        "Trykk '3' for <p>\n" +
                        "Trykk '4' for <blaa>\n");
        String[] reseptListe = { "hvit", "militaer", "p", "blaa" };
        brukerInput = sc.nextInt();
        if (brukerInput == 1 || brukerInput == 2 || brukerInput == 3 || brukerInput == 4) {
            typeResept = reseptListe[brukerInput - 1];
        } else {
            System.out.println("Ugyldig input for resept type: " + brukerInput);
            return;
        }

        System.out.println("Her har vi pasientene: \n");
        for (Pasient p : pasienter) {
            System.out.println(p + "\n");
        }
        System.out.println("Pasient id: \n");
        int tmpID = 0;
        if (!(sc.hasNextInt())){
            System.out.println("Ugyldig input");
            System.exit(0);
        }
        brukerInput = tmpID = sc.nextInt();
        pasient = pasienter.hent(brukerInput);
        if (pasient == null) {
            System.out.println("Pasienten med id: " + brukerInput + " finnes ikke. Legg til ny pasient.\n");
            leggTilElementer();
        }

        System.out.println("Her har vi legemidlene: \n");
        for (Legemiddel l : legemidler) {
            System.out.println(l + "\n");
        }
        System.out.println("Legemiddel id: \n");
        int tmpLegemiddel;
        if (!(sc.hasNextInt())){
            System.out.println("Uglydig input");
        }
        brukerInput = tmpLegemiddel = sc.nextInt();
        legemiddel = legemidler.hent(brukerInput - 1);
        System.out.println(legemiddel + "\n");
        if (legemiddel == null) {
            System.out.println("Legemiddel med id" + brukerInput + " finnes ikke. Legg til nytt legemiddel.\n");
            leggTilElementer();
        }
        int teller = 0;
        if (!(legemiddel instanceof Vanlig)) {
            for (Lege l : leger) {
                if (l instanceof Spesialist) {
                    System.out.println(teller + ": " + l.hentNavn());
                    }
                    teller++;
            }
        } else {
            for (Lege l : leger) {
                System.out.println(teller + ": " + l.hentNavn());
                teller++;
            }
        }
        if (!(sc.hasNextLine())) {
            System.out.println("Uglydig input!");
            System.exit(0);
        }
        int tmpLege;
        brukerInput = tmpLege = sc.nextInt();
        System.out.println("Skriv inn reit for resept: \n");
        if(sc.hasNextInt()) {
            brukerInput = reit = sc.nextInt();
            pushResept(typeResept, tmpLege, tmpLegemiddel, tmpID, reit);
        }
        System.out.println("Resepten er lagt til");
        mainLoop();
            
    }

    public void pushResept(String type, int legePos, int legemidlerPos, int pasientPos, int reit ){
        Lege lege = leger.hent(legePos);
        Legemiddel legemiddel = legemidler.hent(legemidlerPos - 1);
        Pasient pasient = pasienter.hent(pasientPos);
        
        try {
            if (type.equals("militaer")) {
                lege.skrivMilResept(legemiddel, pasient);
            } else if (type.equals("hvit")) {
                lege.skrivHvitResept(legemiddel, pasient, reit);
            } else if (type.equals("p")) {
                lege.skrivPResept(legemiddel, pasient, reit);
            } else if (type.equals("blaa")) {
                lege.skrivBlaaResept(legemiddel, pasient, reit);
            }
        } catch (UlovligUtskrift e) {
            System.out.println("Uglydig utskrift");
            e.printStackTrace();
        }
    }

    public void inputSjekk(Scanner sc, String type) {
        if (type == "string" && sc.hasNextLine() && sc.nextLine().length() == 0) {
            System.out.println("Ugyldig input");
            System.exit(0);
        } else if (type == "int" && !(sc.hasNextInt())) {
            System.out.println("Ugyldig input");
            System.exit(0);
        }
    }

    public void brukResept() {
        Scanner sc = null;
        int brukerInput;
        int tmp;

        sc = new Scanner(System.in); // Ikke bruk sc.close() - forer til error
        System.out.println("Hvilken pasient vil du se resepter for?");
        for (Pasient p : pasienter) {
            System.out.println(p.hentId() + ": " + p.hentNavn() + "(fnr " + p.hentFoedselsnr() + ")");
        }
        inputSjekk(sc, "int");
        brukerInput = tmp = sc.nextInt();
        System.out.println("Valgt pasient: \n" + pasienter.hent(brukerInput) + "\n");
        if (pasienter.hent(tmp).hentStabel().hentStoerrelse() == 0) {
            System.out.println("Pasienten " + pasienter.hent(tmp).hentNavn() + " har ingen resepter.\n");
            mainLoop();
        }
        System.out.println("\nHvilken resept vil du bruke?");
        int nr = 0;
        for (Resept r : pasienter.hent(tmp).hentStabel()) {
            System.out.println(String.format("%s %s (%s reit)", nr, r.hentLegemiddel().hentNavn(), r.hentReit()));
            nr++;
        }
        inputSjekk(sc, "int");
        brukerInput = sc.nextInt();
        System.out.println();
        int nr2 = 0;
        for (Resept r : pasienter.hent(tmp).hentStabel()) {
            if (nr2 == brukerInput) {
                if (r.bruk()) {
                    System.out.println(
                            "Resept " + r.hentLegemiddel().hentNavn() + " er brukt. " + r.hentReit() + " er igjen.\n");
                } else if (r.hentReit() <= 0) {
                    System.out.println("Resept " + r.hentLegemiddel().hentNavn() + " har ingen reit igjen.\n");
                }
                if (r.hentLegemiddel() instanceof Narkotisk){
                    r.hentLege().leggTilNarkotiskBruk();
                    narkotiskBrukt++;
                } else if (r.hentLegemiddel() instanceof Vanedannende) vanedannendeBrukt++;
                mainLoop();
            }
            nr2++;
        }
    }

    // E6
    public void visStatistikk() {
        System.out.println("Antall resepter utskrevet for vanedannende legemidler: " + vanedannendeBrukt);

        System.out.println("\nAntall resepter utskrevet for narkotiske legemidler: " + narkotiskBrukt + "\n");

        for (Lege l : leger){
            if (l.skrivUtNarkotiskBruk() > 0){
                System.out.println("Legen " + l.hentNavn() + " har brukt " + l.skrivUtNarkotiskBruk() + " narkotisk resept(er)");
            }
        }

        int antNark = 0;
        String forrige = "";
        for (Pasient p : pasienter) {
            for (Resept r : p.hentStabel()) {
                    if ((r.hentLegemiddel() instanceof Narkotisk) && r.hentReit() > 0){
                        antNark++;
                        forrige = p.hentNavn();
                    }
                    if (((forrige.equals(p.hentNavn())) && antNark > 0)) {
                        System.out.println("Pasienten " + p.hentNavn() + " har " + r.hentReit() + " narkotisk resepter.");
                        antNark = 0;
                    }
                }
            }
        mainLoop();

        // pasienter som har minst 1 gyldig resept for nark virkemidler 
    }

    public void skrivTilFil() {
        try {
            FileWriter filskriver = new FileWriter("utskrift.txt");
            filskriver.write("# Pasienter (navn, fnr)\n");
            for (Pasient p : pasienter) {
                filskriver.write(p.hentNavn() + "," + p.hentFoedselsnr() + "\n");
            }

            filskriver.write("# Legemidler (navn,type,pris,virkestoff,[styrke])\n");
            String styrke = "";
            for (Legemiddel l : legemidler) {
                filskriver.write(String.format("%s,%s,%s,%s",
                        l.hentNavn(),
                        l.getClass().getName().toLowerCase(), // henter navn paa klassen og gjor den til smaa bokstaver
                                                              // (narkotisk, vanlig, etc)
                        l.hentPris(),
                        (int) l.hentVirkestoff()));

                // Hvis man bruker filskriver.write(n.hentNarkotiskStyrke) kommer det rare tegn
                if (l instanceof Narkotisk) { // hvis l er narkotisk cast peker til narkotisk og skriv narkotisk styrke
                                              // til fil
                    Narkotisk n = (Narkotisk) l;
                    styrke = "," + n.hentNarkotiskStyrke();
                    filskriver.write(styrke);
                }
                if (l instanceof Vanedannende) {
                    Vanedannende v = (Vanedannende) l;
                    styrke = "," + v.hentVanedannendeStyrke();
                    filskriver.write(styrke);
                }
                filskriver.write("\n");

            }

            filskriver.write("# Leger (navn,kontrollid / 0 hvis vanlig lege)\n");
            for (Lege lg : leger) {
                filskriver.write(String.format("%s,",
                        lg.hentNavn()));

                if (lg instanceof Spesialist) {
                    Spesialist s = (Spesialist) lg;
                    filskriver.write(s.hentKontrollID());
                } else {
                    filskriver.write("0");
                }
                filskriver.write("\n");
            }

            filskriver.write("# Resepter (legemiddelNummer,legeNavn,pasientID,type,[reit])\n");
            for (Pasient p : pasienter) {
                for (Resept r : p.hentStabel()) {
                    String type = "";
                    int reit = 0;

                    if (r instanceof MilResept) {
                        type = "militaer";
                    } else if (r instanceof BlaaResept) {
                        type = "blaa";
                        reit = r.hentReit();
                    } else {
                        type = "hvit";
                        reit = r.hentReit();
                    }

                    filskriver.write(String.format("%s,%s,%s,%s",
                            r.hentLegemiddel().hentId(),
                            r.hentLege().hentNavn(),
                            p.hentId(),
                            type));

                    if (reit > 0) {
                        filskriver.write("," + r.hentReit());
                    }
                    filskriver.write("\n");
                }
            }
            filskriver.close();
        } catch (IOException e) {
            System.out.println("Klarte ikke lage fil.");
            e.printStackTrace();
        }
    }
}
