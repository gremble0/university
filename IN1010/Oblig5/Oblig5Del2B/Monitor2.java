import java.util.concurrent.locks.*;
import java.util.HashMap;
import java.util.ArrayList;

public class Monitor2 extends SubsekvensBeholder {
    private Lock laas = new ReentrantLock(true);
    private Condition ikkeTom = laas.newCondition();
    
    public void flett() {
        laas.lock();
        try {
            if (hentStoerrelse() > 1) {
                HashMap<String, Subsekvens> map1 = hentFraIndex(0);
                HashMap<String, Subsekvens> map2 = hentFraIndex(1);
                fjernMap(map1); // fjerner saa samme maps ikke flettes to ganger
                fjernMap(map2);
                HashMap<String, Subsekvens> flettetMap = flettToMaps(map1, map2);
                hentListe().add(flettetMap);
            }
        } finally {
            laas.unlock();
        }
    }
    
    @Override
    public void settInn(HashMap<String, Subsekvens> map) {
        laas.lock();
        try {
            super.settInn(map);
            if (hentStoerrelse() > 1){
                ikkeTom.signal();
            }
        } finally {
            laas.unlock();
        }
    }

    @Override
    public HashMap<String, Subsekvens> hentFraIndex(int index) {
        laas.lock();
        try {
            return super.hentFraIndex(index);
        } finally {
            laas.unlock();
        }
    }

    @Override
    public HashMap<String, Subsekvens> hentTilfeldig() {
        laas.lock();
        try {
            return super.hentTilfeldig();
        } finally {
            laas.unlock();
        }
    }

    @Override
    public void fjernMap(HashMap<String, Subsekvens> map) {
        laas.lock();
        try {
            super.fjernMap(map);
        } finally {
            laas.unlock();
        }
    }

    @Override
    public ArrayList<HashMap<String, Subsekvens>> hentListe() {
        laas.lock();
        try {
            return super.hentListe();
        } finally {
            laas.unlock();
        }
    }

    @Override
    public int hentStoerrelse() {
        laas.lock();
        try {
            return super.hentStoerrelse();
        } finally {
            laas.unlock();
        }
    }
}