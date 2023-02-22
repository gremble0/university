# Definerer det høyeste nivået av klasser i en kjede av sammenkoblede klasser
# klassen består av en ordbok av regneklynge objekter. I tillegg har klassen en
# metode for å 
import sys
from regneklynge import Regneklynge
from node import Node
from rack import Rack

class Datasenter:
    def __init__(self):
        self._regneklynger = {}
    
    def les_fra_fil(self, filnavn):
        # Error i windows uten eksplisitt navngivelse av path til fil, funker uten i linux. Dobbel "\" fordi den må escapes inni f strenger
        fil = open(f"{sys.path[0]}\\{filnavn}")
        klynge_navn = filnavn.split(".")[0] # Navn på klyngen er det som kommer før "." (filendelsen) i filnavnet

        for i, linje in enumerate(fil):
            # Løkker gjennom linjene i filen (racksene) og oppretter dem etter beskrevet 
            linje_split = linje.strip().split()
            if i != 0:
                nytt_rack = Rack()
                # Lagrer i variabler for oversiktlighet, kunne også kalt på linje.split()[x] for hvert bruk av variablene for å spare minne
                antall_noder, minne, prosessorer = int(linje_split[0]), int(linje_split[1]), int(linje_split[2])
                for _ in range(antall_noder): # Løkker gjennom like mange ganger som det står det er av antall noder i fil
                    ny_node = Node(minne, prosessorer)
                    nytt_rack.settInn(ny_node)
                ny_regneklynge.legg_til_rack(nytt_rack)
            else: # Kun for første gjennomgang av løkken
                ny_regneklynge = Regneklynge(linje) # Ved første gjennomgang av løkken er linje maks antall noder
        self._regneklynger[klynge_navn] = ny_regneklynge
    
    # Gitt navnet på en regneklynge, skriv ut informasjon om regneklyngen
    def skrivUt(self, regneklyngeNavn):
        print(f"Informasjon om regneklyngen {regneklyngeNavn}:")
        print(f"Antall rack: {self._regneklynger[regneklyngeNavn].antRacks()}")
        print(f"Antall prosessorer: {self._regneklynger[regneklyngeNavn].antProsessorer()}")
        print(f"Noder med minst 32GB minne: {self._regneklynger[regneklyngeNavn].noderMedNokMinne(32)}")
        print(f"Noder med minst 64GB minne: {self._regneklynger[regneklyngeNavn].noderMedNokMinne(64)}")
        print(f"Noder med minst 128GB minne: {self._regneklynger[regneklyngeNavn].noderMedNokMinne(128)}\n")
    
    def skrivUtAlt(self):
        for regneklynger in self._regneklynger:
            self.skrivUt(regneklynger)