public class Prioritetskoe<T extends Comparable<T>> extends IndeksertListe<T> {
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
                super.leggTil(i, x); // IndeksertListe sin leggTil
                return;
            }
        }
        super.leggTil(x);
    }
}
