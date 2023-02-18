import java.util.ArrayList;

public class Dataklynge {
    private ArrayList<Rack> racks = new ArrayList<Rack>();

    public void leggTilNodeIRack(Node node) {
        for (Rack i : racks) {
            // legger til node i rack hvis ledig plass
            if (i.leggTilNode(node)) { 
                return;
            }
        } 
		
        // folgende kode vil kun kjores hvis det ikke var ledig plass i noen av racksene i dataklyngen
        Rack nyRack = new Rack();
        nyRack.leggTilNode(node);
        racks.add(nyRack); // legger til paa slutten av arraylisten
    }

    public int antProsessorerTotalt() {
        int sum = 0; // maa faa en startverdi
        // lokker gjennom alle racks i instansvariabelen og kaller paa rack sin metode for aa finne antall prosessorer
        for (Rack i : racks) {
            sum += i.antProsessorerIRack();
        }
        return sum;
    }

    public int noderMedNokMinneTotalt(int paakrevdMinne) { 
        int sum = 0;
        // lokker gjennom alle racks i instansvariabelen og kaller paa rack sin metode for aa sjekke minne
        for (Rack i : racks) {
            sum += i.noderMedNokMinneIRack(paakrevdMinne);
        }
        return sum;
    }

    public int antRacks() {
        return racks.size();
    }
}