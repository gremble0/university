# Del av en kjede sammenkoblede klasser
# Denne klassen samler node-objekter i en liste, i tillegg tilbyr den 
# et par hjelpemetoder

class Rack:
    def __init__(self):
        self._noder = []
    
    # Legger til node i liste, forventer node objekt som parameter
    def settInn(self, node):
        self._noder.append(node)
    
    # Returnerer antall noder i instansvariabelen _noder
    def getAntNoder(self):
        return len(self._noder)
    
    def antProsessorer(self):
        sum_prosessorer = 0
        for noder in self._noder:
            sum_prosessorer += noder.antProsessorer()
        return sum_prosessorer
    
    # Returnerer antall noder i rack med mer enn eller lik mengde minne gitt som argument
    def noderMedNokMinne(self, paakrevdMinne):
        antall_noder = 0
        for noder in self._noder: # Løkker gjennom alle noder i racket
            if noder.nokMinne(paakrevdMinne):
                antall_noder += 1 # legger til 1 hvis gjeldende node har påkrevd minne
        return antall_noder