# Programmet definerer en klasse som: initierer instansvariabler, lar brukeren spille av sangen i objektet og
# sjekker om en gitt artist og/eller tittel er det samme som den lagret i objektet.
# 2

class Sang:
    def __init__(self, artist, tittel):
        self._artist = artist
        self._tittel = tittel
    
    # Spiller av sang
    def spill(self):
        print(f"Spiller nå: {self._tittel} - {self._artist}")
    
    # Sjekker om navn (gitt som parameter) er et av ordene i objektets artist variabel
    def sjekkArtist(self, navn):
        navn_split = navn.split()
        artist_split = self._artist.split()
        for navn in navn_split:
            if navn in artist_split: # Hvis parametret navn er det samme som objektets _artist instansvariabel...
                return True
        return False
    
    # Sjekker om tittel (gitt som parameter) er den samme som objektets tittel variabel
    def sjekkTittel(self, tittel):
        return tittel.lower() == self._tittel.lower() # Konverterer begge til små bokstaver

    # Kaller på sjekkArtist og sjekkTittel for å sjekke om både parameteret artist et av ordene i objektets artist variabel
    # og om parameteret tittel er den samme som objektets tittel variabel
    def sjekkArtistOgTittel(self, artist, tittel):
        return self.sjekkArtist(artist) and self.sjekkTittel(tittel) 
        # Hvis både metodene sjekk_artist og sjekk_tittel returnerer True, returneres True, hvis ikke, False