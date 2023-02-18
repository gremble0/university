import java.util.Iterator;
abstract public class Lenkeliste<T> implements Liste<T> {
 
    public class Node {
        private T verdi;
        private Node neste;
 
        public Node(T verdi) {
            this.verdi = verdi;
        }
 
        public Node hentNeste() {
            return neste;
        }
 
        public void settNeste(Node n) {
            neste = n;
        }
 
        public T hentVerdi() {
            return verdi;
        }
    }
    
    public class LenkelisteIterator implements Iterator<T> {
        Node node = forste;
        public T next() {
            T verdi = node.verdi;
            node = node.neste;
            return verdi;
        }
    
        public boolean hasNext() {
            return (node != null);
        }
    }
    
    protected int stoerrelse = 0;
    protected Node forste;
    protected Node siste;
 
    public int stoerrelse() {
        return stoerrelse;
    }
 
    // legger til nytt node objekt bakerst i listen
    public void leggTil(T x) {
        Node nyNode = new Node(x);
        stoerrelse++;
        if (stoerrelse == 1) { // hvis listen var tom
            forste = nyNode;
            siste = nyNode;
            return;
        }
        siste.settNeste(nyNode);
        siste = nyNode;
    }
 
    // henter forste element i lenket liste
    public T hent() {
        return forste.hentVerdi();
    }
    
    // fjerner forste element i lenket liste og returnerer det
    public T fjern() throws UgyldigListeIndeks {
        if (forste == null) { // hvis listen er tom...
            throw new UgyldigListeIndeks(0);
        }
        Node kopi = forste;
        forste = forste.hentNeste(); // setter forstes verdi til aa peke paa det neste elementet
        stoerrelse--;
        return kopi.hentVerdi();
    }
 
    @Override
    public String toString() {
        String resultat = "";
        Node peker = forste;
        for (int i = 0; i < stoerrelse; i++) {
            resultat += peker.hentVerdi();
            peker = peker.hentNeste();
        }
        return resultat;
    }

    public Iterator<T> iterator() {
        return new LenkelisteIterator();
    }

    public int hentStoerrelse() {
        return stoerrelse;
    }
}