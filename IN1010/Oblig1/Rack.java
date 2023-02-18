public class Rack {
    private Node[] noder = new Node[12];

    // Finner en ledig plass til node og returnerer true hvis ledig plass finnes, false hvis ikke
    public boolean leggTilNode(Node node) {
        for (int i = 0; i < 12; i++) {
            // hvis gjeldende element i noder arrayen er null (ikke satt enda) "plasser" noden her
            if (noder[i] == null) { 
                noder[i] = node;                
                return true;
            }
        }
        return false;
    }

    public int antProsessorerIRack() {
        int sum = 0;
        for (int i = 0; i < antNoder(); i++) {
            sum += noder[i].antProsessorerINode();
        }
        return sum;
    }

    public int noderMedNokMinneIRack(int paakrevdMinne) {
        int sum = 0;
        for (int i = 0; i < antNoder(); i++) {
            if (noder[i].nokMinne(paakrevdMinne)) {
                sum++;
            }
        }
        return sum;
    }

    // privat hjelpemetode for oyeblikket brukt av antProsessorerIRack()
    // finner hvor mange av elementene i noder instansvariabelen som er av klassen Node
    private int antNoder() {
        int antall = 0;
        for (int i = 0; i < 12; i++) {
            if(noder[i] instanceof Node) { // sjekker om gjeldende element i array er av klassen Node
                antall++;
            }
        }
        return antall;
    }
}