class Baat {
    private static int antProd = 0;
    private int prodnr;
    private String merke;

    public Baat(String mrk) {
        prodnr = antProd;
        antProd++;
        merke = mrk;
    }

    public String hentInfo() {
        return "Produksjonsnummer: " + prodnr + ", merke: " + merke;
    }    

    public static void main(String[] args) {
        Baathus mittBaathus = new Baathus(3);
        Baat baat1 = new Baat("Opel");
        Baat baat2 = new Baat("Volkswagen");
        Baat baat3 = new Baat("BMW");

        mittBaathus.settInn(baat1);
        mittBaathus.settInn(baat2);
        mittBaathus.settInn(baat3);

        mittBaathus.skrivBaater();
    }
}

class Baathus {
    private Baat[] baater;

    public Baathus(int antallPlasser) {
        baater = new Baat[antallPlasser];
    }
    
    public void settInn(Baat baat) {
        boolean ledigPlass = false;
        for (int i = 0; i < baater.length; i++) {
            if (baater[i] == null) {
                baater[i] = baat;
                ledigPlass = true;
                break;
            }
        }

        if (!ledigPlass) {
            System.out.println("Fant ingen ledige plasser.");
        }
    }

    public void skrivBaater() {
        for (int i = 0; i < baater.length; i++) {
            System.out.println(baater[i].hentInfo());
        }
    }
}

// class TestBaathus {

// }