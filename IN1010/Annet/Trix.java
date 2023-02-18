class Trix {
    double lengde;
    double bredde;
    
    public Trix (double l, double b) {   // Konstruktør
        lengde = l;
        bredde = b;
    }
  
    public void oekLengde (int okning) {    // Oek lengden som angitt
        lengde += okning;
    }
  
    public void oekBredde (int okning) {    // Oek bredden som angitt
        bredde += okning;
    }
  
    public double areal () {     // Beregn mitt areal
        return lengde * bredde;
    }
  
    public double omkrets () {   // Beregn min omkrets
        return 2 * lengde + 2 * bredde;
    }

    public void reduserLengde (int reduksjon) {
        lengde -= reduksjon;
    }

    public void reduserBredde (int reduksjon) {
        bredde -= reduksjon;
    }

    public static void main(String[] args) {
        Trix rekt1 = new Trix(4, 2);
        Trix rekt2 = new Trix(5, 10);
    
        System.out.println(rekt1.areal());
        System.out.println(rekt2.areal());
    
        rekt1.oekLengde(9);
        rekt2.oekBredde(2);
    
        System.out.println(rekt1.omkrets());
        System.out.println(rekt2.omkrets());

        rekt1.reduserBredde(1);
        rekt1.reduserLengde(4);

        System.out.println(rekt1.areal());
        System.out.println(rekt1.omkrets());
    }
}