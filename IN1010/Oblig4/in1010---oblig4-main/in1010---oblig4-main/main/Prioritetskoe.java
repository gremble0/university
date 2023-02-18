public class Prioritetskoe<T extends Comparable<T>> extends Lenkeliste<T> {
    @Override
    public void leggTil(T x) {
        if (stoerrelse == 0) {
            super.leggTil(x); // Lenkeliste sin leggTil
            return;
        }
        Node kopi = forste;
        for (int i = 0; i < stoerrelse; i++) {
            if (i != 0) {
                kopi = kopi.hentNeste();
            }
            if (kopi.hentVerdi().compareTo(x) > 0) {
                kopiIndeksertLeggTil(i, x); // IndeksertListe sin leggTil
                return;
            }
        }
        super.leggTil(x);
    }

    // brukes siden prioritetskoe sin leggTil metode har mange likheter med indeksertListe sin
    public void kopiIndeksertLeggTil(int pos, T x) throws UgyldigListeIndeks {
        if (pos > stoerrelse || pos < 0) {
            throw new UgyldigListeIndeks(pos);
        }

        Node nyNode = new Node(x);
        Node kopi = forste;
        stoerrelse++;

        if (pos == 0) { 
            if (stoerrelse == 1) {siste = nyNode;} 
            forste = nyNode;
            nyNode.settNeste(kopi);
            return;
        }

        for (int i = 0; i < pos - 1; i++) {
            kopi = kopi.hentNeste();
        }
        nyNode.settNeste(kopi.hentNeste());
        kopi.settNeste(nyNode);

        if (pos == stoerrelse-1) {
            siste = nyNode;
        }
    }

    public T hent(int pos) throws UglydigListeIndeks { 
        if (pos > stoerrelse || pos < 0) {
            throw new UglydigListeIndeks(pos);
        }

        Node kopi = forste;
        // her vil vi bare ha det paa pos og er ikke interessert i det paa pos-1 eller pos+1
        // - dermed i < pos i stedet for i < pos - 1
        for (int i = 0; i < pos; i++) {
            kopi = kopi.hentNeste();
        }
        return kopi.hentVerdi();
    }
}
