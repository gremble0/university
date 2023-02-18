import java.io.FileNotFoundException;
import java.io.File;
import java.util.Scanner;

public class Legesystem {
    IndeksertListe<Pasient> pasienter = new IndeksertListe<>();
    IndeksertListe<Legemiddel> legemidler = new IndeksertListe<>();
    Prioritetskoe<Lege> leger = new Prioritetskoe<>();

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

    public void leggTilLinje(String type, String linje, Scanner leser) { // maa sende med linje for aa ha tilgang til forrige linje
        String[] linje_split_komma = linje.trim().split(",");
        String[] linje_split_mellomrom = linje.trim().split(" ");

        if (linje_split_mellomrom[0].equals("#")) {
            linje = leser.nextLine();
            linje_split_komma = linje.trim().split(",");
        }

        if (type.equals("Pasienter")) {
            pasienter.leggTil(new Pasient(linje_split_komma[0], linje_split_komma[1])); // bruker Lenkeliste sin leggTil()
        } 
        
        else if (type.equals("Legemidler")) {
            if (linje_split_komma[1].equals("narkotisk")) {
                legemidler.leggTil(new Narkotisk(linje_split_komma[0], Integer.parseInt(linje_split_komma[2]), Double.parseDouble(linje_split_komma[3]),
                        Integer.parseInt(linje_split_komma[4])));
            } else if (linje_split_komma[1].equals("vanedannende")) {
                legemidler.leggTil(new Vanedannende(linje_split_komma[0], Integer.parseInt(linje_split_komma[2]),
                        Double.parseDouble(linje_split_komma[3]), Integer.parseInt(linje_split_komma[4])));
            } else if (linje_split_komma[1].equals("vanlig")) {
                legemidler.leggTil(new Vanlig(linje_split_komma[0], Integer.parseInt(linje_split_komma[2]), Double.parseDouble(linje_split_komma[3])));
            }
        } 
        
        else if (type.equals("Leger")) {
            if (Integer.parseInt(linje_split_komma[1]) != 0) {
                leger.leggTil(new Spesialist(linje_split_komma[0], linje_split_komma[1]));
            } else {
                leger.leggTil(new Lege(linje_split_komma[0]));
            }
        } 
        
        else if (type.equals("Resepter")) {
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
                Resept resept = new BlaaResept(legemiddel, lege,pasient, Integer.parseInt(linje_split_komma[4]));
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

    public void qwerty(){
        // System.out.println(pasienter.hentStoerrelse());
        // System.out.println(pasienter);
        // System.out.println();
        // System.out.println(legemidler.hentStoerrelse());
        // System.out.println(legemidler);
        System.out.println(leger.hentStoerrelse());
        System.out.println(leger);
        // for (Lege l : leger){
        //     System.out.println(l);
        // }
    }

    public void mainLoop() {
        Scanner sc = null;
        String brukerInput = "";
        while(!brukerInput.equals("q")) {
            sc = new Scanner(System.in);

            System.out.println(
                "Her har vi noen valgmuligheter:\n" +
                "Trykk '1' for aa skrive ut alle elementer i legesystemet.\n" +
                "Trykk '2' for aa lage og legge til nye elementer i systemet.\n" +
                "Trykk '3' for aa bruke en resept til en pasient.\n" +
                "Trykk '4' for aa åpne undermeny for vis av statistikk.\n" +
                "Trykk '5' for aa skrive all data til fil.\n" +
                "Trykk 'q' for aa avslutte programmet.\n"
            );

            brukerInput = sc.nextLine();
            System.out.println("test1");
            if(brukerInput.equals("1")) {
                System.out.println("tester 1 input");
                skrivUtAlleElementer(); //E3
            } else if(brukerInput.equals("2")) {
                //TODO E4
            } else if(brukerInput.equals("3")) {
                //TODO E5
            } else if(brukerInput.equals("4")) {
                //TODO E6
            } else if(brukerInput.equals("5")) {
                //TODO E7
            }
        }
        sc.close();

    }
    public void skrivUtAlleElementer() {
        System.out.println("test2");
        for (Pasient p : pasienter) {
            System.out.println(p+"\n");
        }
        for (Legemiddel l : legemidler) {
            System.out.println(l+"\n");
        }
        for (Lege l : leger) {
            System.out.println(l+"\n");
        }
        for (Pasient r : pasienter) {
            System.out.println(r.hentNavn()+"\n");
        }
    }
}
