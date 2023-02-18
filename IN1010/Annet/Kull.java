abstract class Kull implements Iterable<Hund> {
	Hund mor, far;

	Kull (Hund mor, Hund far) {
		this.mor = mor;
		this.far = far;
	}

	public abstract void settInn(Hund h);
	public abstract Iterator<Hund> iterator();    
}
