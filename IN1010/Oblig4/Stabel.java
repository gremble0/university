

public class Stabel<T> extends Lenkeliste<T> {
    @Override
    public void leggTil(T x) {
        // Lager nye node-objekter for det nye forste objektet og det gamle
        Node nyNode = new Node(x);
        Node kopi = forste;
        // Setter det forste objektet lik det nye objektet og gjor at den peker paa det tidligere forste objektet
        forste = nyNode;
        forste.settNeste(kopi);
        stoerrelse++;
    }

}