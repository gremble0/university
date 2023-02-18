import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Hovedprogram {
    public static void main(String[] args) {
        Dataklynge nyKlynge = new Dataklynge();
        try {
            File fil = new File("dataklynge2.txt");
            Scanner leser = new Scanner(fil);
            while (leser.hasNextLine()) {
                String[] linje = leser.nextLine().split(" ", 0);
                int[] linjeTall = new int[linje.length];
                // lokker gjennom strings i linjen og konverterer til int
                for (int i = 0; i < linje.length; i++) {
                    linjeTall[i] = Integer.parseInt(linje[i]);
                }

                // linjetall[0] = antall noder, linjetall[1] = antall prosessor per node, linjetall[2] = minne per node
                for (int i = 0; i < linjeTall[0]; i++) {
                    Node tempNode = new Node(linjeTall[1], linjeTall[2]); 
                    nyKlynge.leggTilNodeIRack(tempNode);
                }
            }
            
        System.out.println("Noder med minst 128 GB: " + nyKlynge.noderMedNokMinneTotalt(128));
        System.out.println("Noder med minst 512 GB: " + nyKlynge.noderMedNokMinneTotalt(512));
        System.out.println("Noder med minst 1024 GB: " + nyKlynge.noderMedNokMinneTotalt(1024));
        System.out.println("Antall prosessorer: " + nyKlynge.antProsessorerTotalt());
        System.out.println("Antall Rack: " + nyKlynge.antRacks());
        
        } catch (FileNotFoundException e) {
            System.out.println("Fant ikke filen.");
            e.printStackTrace();
        }
    }
}