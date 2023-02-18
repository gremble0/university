public class IndeksertListe<T> extends Lenkeliste<T> {
    public void leggTil(int pos, T x) throws UgyldigListeindeks {
        if (pos > stoerrelse || pos < 0) {
            throw new UgyldigListeindeks(pos);
        }

        Node nyNode = new Node(x); // nyNode skal bli pekt paa av pos-1 og skal peke paa pos+1
        Node kopi = forste;
        stoerrelse++;

        if (pos == 0) { // vi skal sette noden forrerst i listen
            if (stoerrelse == 1) {siste = nyNode;} // listen er tom
            forste = nyNode;
            nyNode.settNeste(kopi);
            return;
        }

        for (int i = 0; i < pos - 1; i++) {
            kopi = kopi.hentNeste();
        }
        // kopi er node paa pos
        nyNode.settNeste(kopi.hentNeste()); // nyNode peker paa node paa pos+1
        kopi.settNeste(nyNode); // node paa pos-1 peker naa paa nyNode

        if (pos == stoerrelse-1) {
            siste = nyNode;
        }
    }

    public void sett(int pos, T x) throws UgyldigListeindeks {
        if (pos >= stoerrelse || pos < 0) { 
            throw new UgyldigListeindeks(pos);
        }

        Node nyNode = new Node(x);
        Node kopi = forste;
        for (int i = 0; i < pos - 1; i++) {
            kopi = kopi.hentNeste();
        }
        nyNode.settNeste(kopi.hentNeste().hentNeste());
        kopi.settNeste(nyNode);

        if (pos == stoerrelse) {
            siste = nyNode;
        }
    }

    public T hent(int pos) throws UgyldigListeindeks { 
        if (pos > stoerrelse || pos < 0) {
            throw new UgyldigListeindeks(pos);
        }

        Node kopi = forste;
        // her vil vi bare ha det paa pos og er ikke interessert i det paa pos-1 eller pos+1
        // - dermed i < pos i stedet for i < pos - 1
        for (int i = 0; i < pos; i++) {
            kopi = kopi.hentNeste();
        }
        return kopi.hentVerdi();
    }

    public T fjern(int pos) throws UgyldigListeindeks {
        if (pos >= stoerrelse || pos < 0) {
            throw new UgyldigListeindeks(pos);
        }

        Node kopi = forste;
        for (int i = 0; i < pos - 1; i++) {
            kopi = kopi.hentNeste();
        }
        // lagrer kopi av verdien til noden vi skal fjerne og setter neste verdien
        // til noden foer den til noden etter den
        T kopisVerdi = kopi.hentNeste().hentVerdi();
        kopi.settNeste(kopi.hentNeste().hentNeste());
        stoerrelse--;
        return kopisVerdi;
    }

    @Override
    public String toString() { 
        String resultat = "";
        Node temp = forste;
        for (int i = 0; i < stoerrelse; i++) {
            resultat += temp.hentVerdi() + ", ";
            temp = temp.hentNeste();
        }
        resultat += " Forste: " + forste.hentVerdi() + ", Siste: " + siste.hentVerdi();
        return resultat;
    }
}