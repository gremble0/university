# Programmet definerer en klasse for hunder med metoder for å: initiere instansvariabler, hente alder og vekt,
# la hunden løpe og trekke fra vekt hvis hunden ikke er mett, la hunden spise og legge til vekt hvis hunden 
# allerede var mett da den spiste
# 3.1-4

class Hund:
    def __init__(self, alder, vekt): # Initierer instansvariabler
        self.alder = alder
        self.vekt = vekt
        self.metthet = 10
    
    def hent_alder(self):
        return self.alder
    
    def hent_vekt(self):
        return self.vekt
    
    def spring(self):
        self.metthet -= 1 # Trekk fra metthet
        if self.metthet < 5:
            self.vekt -= 1 # Trekk fra vekt

    def spis(self, mat):
        self.metthet += mat # Legg til metthet
        if self.metthet > 7:
            self.vekt += 1 # Legg på vekt
    