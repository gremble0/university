# 

from rack import Rack

class Regneklynge:
    def __init__(self, noderPerRack):
        self._racks = []
        self._maks_noder = noderPerRack # Maks noder per rack
    
    # Plasserer node i et rack med ledig plass, lager nytt rack hvis ingen har ledig plass
    def settInnNode(self, node):
        for racks in self._racks: # Løkker gjennom racks
            if racks.getAntNoder() < self._maks_noder:
                racks._noder.append(node)
                return # Returnerer (avslutter) etter å ha lagt til node i ledig rack
        # lager nytt rack, legger den til i listen over racks og legger node inni den hvis ingen var ledige
        nytt_rack = Rack()
        nytt_rack.settInn(node)
        self._racks.append(nytt_rack)
    
    # Legger til rack i liste, forventer rack objekt som parameter
    def legg_til_rack(self, rack):
        self._racks.append(rack)

    # Returnerer antall prosessorer i regneklynge
    def antProsessorer(self):
        sum_prosessorer = 0
        for racks in self._racks: # Løkker gjennom alle racks i regneklynge
            sum_prosessorer += racks.antProsessorer() # Legger antall prosessorer i gjeldende rack til summen
        return sum_prosessorer

    # Returnerer antall noder med nok minne i alle av regneklynge-objektets racks
    def noderMedNokMinne(self, paakrevdMinne):
        antall_noder = 0
        for racks in self._racks:
            antall_noder += racks.noderMedNokMinne(paakrevdMinne)
        return antall_noder
    
    # Returnerer antall racks i regneklynge-objektet
    def antRacks(self):
        return len(self._racks)