import java.util.HashMap;

abstract public class Rute {
    protected int radNr;
    protected int kolonneNr;
    protected Labyrint labyrintRef;
    protected HashMap<String, Rute> naboer;

    public Rute(int radNr, int kolonneNr, Labyrint labyrintRef) {
        this.radNr = radNr;
        this.kolonneNr = kolonneNr;
        this.labyrintRef = labyrintRef;
        this.naboer = new HashMap<String, Rute>();
    }

    public void settNabo(String naboRetning, Rute rute) {
        naboer.put(naboRetning, rute);
    }

    public void finn(Rute fra) {}
}

class HvitRute extends Rute {
    public HvitRute(int radNr, int kolonneNr, Labyrint labyrintRef) {
        super(radNr, kolonneNr, labyrintRef);
    }

    @Override
    public String toString() {
        return " .";
    }

    @Override
    public void finn(Rute fra) {
        if (naboer.get("nord") != fra && naboer.get("nord") != null) {
            naboer.get("nord").finn(this);
        }
        if (naboer.get("oest") != fra && naboer.get("oest") != null) {
            naboer.get("oest").finn(this);
        }
        if (naboer.get("soer") != fra && naboer.get("soer") != null) {
            naboer.get("soer").finn(this);
        }
        if (naboer.get("vest") != fra && naboer.get("vest") != null) {
            naboer.get("vest").finn(this);
        }
    }

    public void hentNabo() {
        System.out.print(" nord: " + naboer.get("nord"));
        System.out.print(" oest: " + naboer.get("oest"));
        System.out.print(" soer: " + naboer.get("soer"));
        System.out.print(" vest: " + naboer.get("vest"));
    }
}

class Aapning extends HvitRute {
    public Aapning(int radNr, int kolonneNr, Labyrint labyrintRef) {
        super(radNr, kolonneNr, labyrintRef);
    }

    @Override
    public void finn(Rute fra) {
        System.out.println(String.format("Fant aapning ved: (%d,%d)", radNr, kolonneNr));
        super.finn(fra); // i tilfelle vi starter paa en aapning
    }
}

class SortRute extends Rute {
    public SortRute(int radNr, int kolonneNr, Labyrint labyrintRef) {
        super(radNr, kolonneNr, labyrintRef);
    }

    @Override
    public String toString() {
        return " #";
    }

    @Override
    public void finn(Rute fra) {
        if (fra == this) {
            System.out.println("Kan ikke starte i sort rute");
        }
        return; // ikke gyldig vei saa slutt aa lete
    }
}