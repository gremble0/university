# Definerer det laveste nivået av klasser i en kjede sammenkoblede klasser
# Klassen er en node med gitt mengde minne og antall prosessorer, med et par 
# hjelpemetoder for å redigere og returnere instansvariabler

class Node:
    def __init__(self, minne, prosessorer):
        self._minne = minne
        self._prosessorer = prosessorer

    # Legger til minne
    def legg_til_minne(self, minne):
        self._minne += minne

    # Legger til prosessor(er)
    def legg_til_prosessor(self, prosessorer):
        self._prosessorer += prosessorer
    
    # Returnerer antall prosessorer
    def antProsessorer(self):
        return self._prosessorer
    
    # Returnerer antall minne i GB
    def antMinne(self):
        return self._minne
    
    def nokMinne(self, paakrevdMinne):
        # Returnerer True om objektets minne er større enn eller lik paakrevdMinne
        # Ellers returneres False
        return self._minne >= paakrevdMinne