interface kjokken {
    int kjokkenRomNr;
}

abstract class Rom {
    final int romNr;
    final int kvadratMr;
    final int antallSengeplasser;
    final int etasje;
    boolean erLedig = true;
    final Rom nesteRom;
    
    public Rom(int romNr, int kvadratMr, int antallSengeplasser, int etasje, Rom nesteRom) {
        this.romNr = romNr;
        this.kvadratMr = kvadratMr;
        this.antallSengeplsasser = antallSengeplasser;
        this.etasje = etasje;
        this.nesteRom = nesteRom // kan vaere null
    }
    
    @Override
    public String toString() {
        String erLedigTekst = "";
        if (!erLedig) erLedigTekst = "ikke";
        return (String.format(
            "Dete er rommet med romNr %d, det er et %s rom, den er %dm^2, har %d sengeplasser, ligger i %d. etasje, er %s ledig og det neste rommet er romet med rom nummer %s.", romNr, getClassName(), kvadratMr, antallSengeplasser, etasje, erLedigTekst, nesteRom.romNr
            )); // blir ikke noe saerlig fin utskrift med getClassName, men det viser all informasjonen
    }
}

class Vanlig extends Rom implements Kjokken {
    int kjokkenRomNr;
    
    public Vanlig(int romNr, int kjokkenRomNr, int kvadratMr, int antallSengeplasser, int etasje, Rom nesteRom) {
        super(romNr, kvadratMr, antallSengeplasser, etasje, nesteRom);
        this.kjokkenRomNr = kjokkenRomNr;
    }
}

class Suite extends Rom implements Kjokken {
        int kjokkenRomNr;
    
    public Suite(int romNr, int kjokkenRomNr, int kvadratMr, int antallSengeplasser, int etasje, Rom nesteRom) {
        super(romNr, kvadratMr, antallSengeplasser, etasje, nesteRom);
        this.kjokkenRomNr = kjokkenRomNr;
    }
}

class Enkelt extends Rom {
    public Enkelt(int romNr, int kvadratMr, int antallSengeplasser, int etasje, Rom nesteRom) {
        super(romNr, kvadratMr, antallSengeplasser, etasje, nesteRom);
    }
}



class HotellEnEtasje { // blir senere utvidet av klassen Hotell
    Rom forsteRom;
    int antallRom;
    final int MAX_ANT_SENGEPLASSER = 8;
    Reservasjon forsteR;
    Reservasjon sisteR;
    
    //public HotellEnEtasje(int antallRom) { // maa oppgi antall rom i hotellet for at programmet skal virke
        //this.antallRom = antallRom;
        //Rom temp = forsteRom;
        //for (int i = 0; i < antallRom; i++) {
            //temp.etasje = 1;
            //temp = temp.nesteRom;
        //}
    //}
    
    public void tildelRom(String navn) {
        Reservasjon temp = forsteR;
        while (true) {
            if (temp.gjesteRef.navn == navn) {
                taUtRes(temp);
                Rom bestiltRom;
                while (bestiltRom == null) {
                    bestiltRom = finnRom(temp.onskedeSengeplasser, onskerKjokken);
                    if (temp.onskedeSengeplasser == MAX_ANT_SENGEPLASSER) {
                        throw new IngenMuligeRomException("Ingen mulige rom for " + navn + " som oensker " + temp.onskedeSengeplasser + " sengeplasser");
                    }
                    temp.onskedeSengeplasser += 1;
                }
                bestiltRom.erLedig = false;
                System.out.printLn(bestiltRom);
                break;
            }
            temp = temp.nesteR;
            
            if (temp.gjesteRef.nesteR == null) {
                throw new FantIkkeGjestException("Gjesten har ikke bestilt et rom");
            }
        }
    }
    
    rom finnRom(int antSeng, boolean kjokken) {
        Rom temp = forsteRom;
        while (true) {
            if (kjokken) {
                if (temp.antallSengeplasser == antSeng && temp instanceof Kjokken) {
                    return temp;
                }
            } else {
                if (temp.antallSengeplasser == antSeng) {
                    return temp;
            }
            if (temp.nesteRom == null) {
                return null;
            }
            temp = temp.nesteRom;
        }
    }
    
    void taUtRes(Reservasjon r) {
        Reservasjon temp = forsteR;
        while (true) {
            try {
                if (temp == r) {
                    temp.forrigeR.nesteR = temp.nesteR;
                    temp.nesteR.forrigeR = temp.forrigeR,
                    return;
            } catch (NullPointerException e) {} // i tilfelle vi proever aa ta ut den forste eller siste reservasjonen hadde vi faatt error naar vi proever aa hente null (temp.forrigeR) sin nesteR
            temp = tmep.nesteR;
        }
    }
}

class FantIkkeGjestException extends RuntimeException {
    public FantIkkeGjestException(String feilmelding) {
        super(feilmelding);
    }
}

class IngenMuligeRomException extends RuntimeException {
    public IngenMuligeRomException(String feilmelding) {
        super(feilmelding);
    }
}

class Gjest {
    String navn;
    Rom romRef;
    
    public Gjest(String navn, Rom romRef) {
        this.navn = navn;
        this.romRef = romRef;
    }
}

class Reservasjon {
    Gjest gjestRef;
    int onskedeSengeplasser;
    boolean onskerKjokken;
    Reservasjon forrigeR;
    Reservasjon nesteR;
    
    public Reservasjon(Gjest gjestRef, int onskedeSengeplasser, boolean onskerKjokken) {
        this.gjestRef = gjestRef;
        this.onskedeSengeplasser = onskedeSengeplasser;
        this.onskerKjokken = onskerKjokken;
    }
}


import.java.util.iterator;

class Hotell { // har ikke skrevet opp variabler og metoder fra hotellEnEtasje som ikke blir brukt i opgavene 9-12
    Rom forsteRom;
    final int ANTALL_ETASJER;
    final int MAX_ANTALL_SENGEPLASSER = 8;
    Rom[] forsteRomEtasje;
    public Hotell() {
        
    }
    
    public int[] ledigeRom() { // oppgave 11
        int[MAX_ANTALL_SENGEPLASSER] resultat; // array av ints med plass til MAX_ANTALL_SENGEPLASSER antall verdier
        Rom temp = forsteRom;
        while (true) {
            resultat[temp.antallSengeplasser - 1]++;
            if (temp.nesteRom == null) {
                return resultat; 
            }
            temp = temp.nesteRom;
        }
    }
    
    public int[] ledigeRomMedIterator() { // oppgave 12
        RomIterator romIt = new RomIterator(forsteRom);
        int[MAX_ANTALL_SENGEPLASSER] resultat;
        while (romIt.hasNext()) {
            resultat[romIt.next.antallSengeplasser - 1]++;
        }
        return resultat;
    }
    
    class RomIterator implements Iterable<Rom> {
        Rom gjeldende = null;
        
        public RomIterator(Rom forste) {
            gjeldende = forste;
        }
        
        @Override
        public boolean hasNext() {
            return gjeldende != null;
        }
        
        @Override
        public Rom next() {
            Rom returverdiRom = gjeldende;
            if (gjeldende.nesteRom != null) {
                gjeldende = gjeldende.nesteRom;
            }
            return returverdiRom;
        }
    }
}

import java.util.concurrent.locks.*;
class HotellMonitor {
    Lock laas = new Reentrantlock(true);
    int[] ledigeRom;
    
    public void rapporterLedigeRom(int[] ledige) {
        laas.lock();
        try {
            ledigeRom = ledige;
        } finally {
            laas.unlock();
        }
    }
    
    public int[] hentLedigeRom() {
        laas.lock();
        try {
            return ledigeRom;
        } finally {
            laas.unlock();
        }
    }
}

class RomTraad implements Runnable {
    HotellMonitor monitorRef;
    Hotell hotellRef;
    
    public RomTraad(Hotell hotellRef) {
        this.hotellRef = hotellRef;
    }
    
    public void run() {
        int[] oppdatertLedigeRom;
        while (true) { // staar i oppgaveteksten "naar en traad kjoeres skal den...", men man bruker gjerne traader til aa kontinuerlig oppdatere noe saa la det traaden gjoer inn i en while true. Hvis man heller vil gjoere dette i et eventuelt hovedprogram eller ikke i det hele tatt, kan man bare fjerne den.
            oppdatertLedigeRom = hotellRef.ledigeRom(); // evt. hotellRef.ledigeRomMedIterator
            monitorRef.rapporterLedigeRom(oppdatertLedigeRom);
        }
    }
}


class HotellKjede {
    ...
    public void skrivUtLedigeRomMedTrader() {
        HotellMonitor monitor = new HotellMonitor;
        Thread[] traader = new Thread[ANTALL_HOTELLER];
        for (int i = 0; i < ANTALL_HOTELLER; i++) {
            traader[i] = new Thread(new RomTraad(alleHoteller[i]);
            traader[i].start;
        }
        for (Thread traad : traader) {
            traad.join
        }
        System.out.println(monitor.ledigeRom);
    }
    ...
}







