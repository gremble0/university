class Tidspunkt implements Comparable<Tidspunkt> {
    // gjoer alle instansvariablene public saann at vi kan sammenligne dem i compareTo metoden uten aa lage get funksjoner
    // ikke slik man hadde gjort det i et vanlig program siden det er mindre sikkert, men denne maaten er kortere.
    public int aar; 
    public int mnd;
    public int dag; 
    public int time;
    public int min;
    public int sek;

    Tidspunkt(int aar, int mnd, int dag, int time, int min, int sek) {
        this.aar = aar;
        this.mnd = mnd;
        this.dag = dag;
        this.time = time;
        this.min = min;
        this.sek = sek;
    }

    public int compareTo(Tidspunkt tidspunkt) {
        if (this.aar < tidspunkt.aar) { // kan gjoeres siden instansvariabelen er public
            return -1;
        } else if (this.aar > tidspunkt.aar) {
            return 1;
        }
        if (this.mnd < tidspunkt.mnd) {
            return -1;
        } else if (this.mnd > tidspunkt.mnd) {
            return 1;
        }
        if (this.dag < tidspunkt.dag) {
            return -1;
        } else if (this.dag > tidspunkt.dag) {
            return 1;
        }
        if (this.time < tidspunkt.time) {
            return -1;
        } else if (this.time > tidspunkt.time) {
            return 1;
        }
        if (this.min < tidspunkt.min) {
            return -1;
        } else if (this.min > tidspunkt.min) {
            return 1;
        }
        if (this.sek < tidspunkt.sek) {
            return -1;
        } else if (this.sek > tidspunkt.sek) {
            return 1;
        }
        return 0;
    }
}