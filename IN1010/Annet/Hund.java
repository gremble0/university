class Hund implements Comparable<Hund> {
    String navn;
    Kull mittKull;
    Tidspunkt minFodselstid;
    Hund neste = null;

    Hund(Kull k, String navn, Tidspunkt fodt) {
    this.navn = navn;
    mittKull = k;
    minFodselstid = fodt;
    }

    @Override
    public int compareTo(Hund h) {
        return minFodselstid.compareTo(h.minFodselstid);
    }

    public Hund mor() {
        return mittKull.mor;
    }

    public Hund far () {
        return mittKull.far;
    }

    public boolean erHelsosken(Hund h) {
	if (mor() == h.mor() && far() == h.far()) {
		return true;
	}
	return false;
    }

    public boolean erHalvsosken(Hund h) {
		int teller = 0;
		if (mor() == h.mor()) {
			teller++;
		}
		if (far() == h.far()) {
			teller++;
		}

		if (teller == 1) {
			return true;
		}
		return false;
    }

    public Hund finnEldsteKjenteOpphav() {
		// ser at det er en "neste" instansvariabel i Hund klassen, men siden hver hund har baade far og mor
		// lagde jeg en array med de to i stedet.
		// loesningen antar ogsaa at det bare er en hund som baade ikke har en far og ikke har en mor, som gjoer den til en
		// ikke perfekt loesning siden den ikke sammenligner hunden med eldst opphav fra fars side og mors side med compareTo

		Hund[] foreldre = { far(), mor() }; // array med lengde 2 for denne hundens foreldre
		
		if (foreldre[0] != null) {
			foreldre[0].finnEldsteKjenteOpphav();
		}
		if (foreldre[1] != null) {
			foreldre[1].finnEldsteKjenteOpphav();
		}

		if (foreldre[0].far() == null && foreldre[0].mor() == null) {
			return foreldre[0];
		} else if (foreldre[1].far() == null && foreldre[1].mor() == null) {
			return foreldre[1];
		}
	}
}


// aa printe ut: kort cheat sheet + Scanner info fra lang cheat sheet + v2019 loesningsforslag + v2019 oppgavesett