# Programmet definerer en klasse for spillelister, en klasse som skal bestå av flere Sang objekter.
# klassen skal: initiere instansvariabler, lese inn data fra en fil ogoppretter objekter basert på 
# linjenes innhold, legger til sang i spilleliste, fjerner sang fra spilleliste, spiller av sang
# fra spilleliste, spiller av alle sanger fra spilleliste, sjekker om sang er i spilleliste,
# returnerer alle sanger med en gitt artist.
import sys
from sang import Sang

class Spilleliste:
    def __init__(self, listenavn):
        self._sanger = []
        self._navn = listenavn
    
    # Leser inn data fra fil og opprett
    def lesFraFil(self, filnavn):
        # Leser sanger fra fil og legger dem til i spilleliste
        fil = open(f"{sys.path[0]}\\{filnavn}", "r") # Error i windows uten eksplisitt navngivelse av path til fil, funker uten i linux (og macOS?)
        for linje in fil:
            linje_split = linje.strip().split(";") # Fjerner "\n" og splitter ved semikolon, returnerer en liste
            tittel = linje_split[0] # Lagrer første element fra den splittede strengen
            artist = linje_split[1] # Andre element
            ny_sang = Sang(artist, tittel) # Oppretter objekt fra variabler lest fra fil
            self.leggTilSang(ny_sang)
        
    # Legger sang inn i spilleliste
    def leggTilSang(self, ny_sang):
        # Forventer sang objekt som argument og legger til sang i spilleliste
        self._sanger.append(ny_sang)
    
    # Fjerner sang fra spilleliste
    def fjernSang(self, sang):
        # Forventer sang objekt som argument og fjerner sang fra spilleliste
        self._sanger.remove(sang)

    # Spiller av sang
    def spillSang(self, sang):
        # Forventer sang objekt som argument, og spiller av sangen
        sang.spill()
    
    # Spiller av alle sanger
    def spillAlle(self):
        # Spiller av alle sanger i spilleliste
        for sanger in self._sanger:
            self.spillSang(sanger)
    
    # Sjekker om gitt tittel finnes i spilleliste og returnerer den hvis den gjør det
    def finnSang(self, tittel):
        # Returnerer Sang objekt ved første forekomst av tittel i spilleliste
        for sanger in self._sanger:
            if sanger.sjekkTittel(tittel): # Hvis gjeldende sang objekts sjekkTittel metode returnerer True med tittel som argument...
                return sanger
    
    # Løkker gjennom sanger og legger til sanger der artistnavnet gitt som parameter er et ord i
    # objektets artist variabel i en liste. Returnerer listen.
    def hentArtistUtvalg(self, artistnavn):
        sanger_med_artist = []
        for sanger in self._sanger:
            if sanger.sjekkArtist(artistnavn):# Hvis gjeldende sang objekts sjekkArtist metode returnerer True med artistnavn som argument...
                sanger_med_artist.append(sanger)
        return sanger_med_artist # Returnerer liste av Sang objekter