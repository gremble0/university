import java.util.Iterator;

class KullListe extends Kull implements Iterable<Hund> {
	Hund forste = null;
	int stoerrelse = 0;

	KullListe (Hund mor, Hund far) {
		super(mor, far);
	}

//	class Node {
//		Hund verdi;
//
//		Node(Kull k, String navn, Tidspunkt fodt) {
//			verdi = new Hund(k, navn, fodt);
//		}
//	}

	public void settInn(Hund h) {
		stoerrelse++;
		if (forste == null) {
			forste = h;
			return;
		}

		if (h.compareTo(forste) > 0) {
			h.neste = forste;
			forste = h;
			return;
		}
		
		Hund ref = forste;
		for (int i = 0; i < stoerrelse - 1; i++) { // -1 siden vi allerede har gjort stoerrelse++
			if (i == stoerrelse - 2) { // hvis vi er paa siste element i listen og h ikke har vaert mindre enn noenting enda
				ref.neste = h;
			}
			if (ref.neste.compareTo(h) < 0) {
				ref.neste = h;
				h.neste = ref.neste.neste;
				return;
			}
			ref = ref.neste;
		}
	}

	@Override
	public Iterator<Hund> iterator() {
	
	@Override
	public boolean hasNext() {
		if (
	}
}
