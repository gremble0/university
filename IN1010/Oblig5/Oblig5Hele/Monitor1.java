import java.util.concurrent.locks.*;
import java.util.HashMap;
import java.util.ArrayList;

// ikke i bruk, kan eventuelt erstattes med Monitor2 i LeseTrad

public class Monitor1 extends SubsekvensBeholder {
    private Lock laas = new ReentrantLock(true);
    private SubsekvensBeholder beholder = new SubsekvensBeholder();

    public void settInn(HashMap<String, Subsekvens> map) {
        laas.lock();
        try {
            beholder.settInn(map);
        } finally {
            laas.unlock();
        }
    }

    @Override
    public ArrayList<HashMap<String, Subsekvens>> hentListe() {
        return beholder.hentListe();
    }

    public int hentStoerrelse() {
        return beholder.hentListe().size();
    }
}
