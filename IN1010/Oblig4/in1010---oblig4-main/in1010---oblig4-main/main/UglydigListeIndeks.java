public class UglydigListeIndeks extends RuntimeException{
    UglydigListeIndeks (int indeks) {
        super("Ugyldig indeks: "+indeks);
    }
}