# Programmet lager en klasse for motorsyklermed metoder for å: initiere instansvariabler, øke kilometerstanden, returnere 
# kilometerstanden og printe ut alle instansvariablene formatert.
# 1.1

class Motorsykkel:
    def __init__(self, merke, reg_nummer, km_stand): # Initierer instansvariabler
        self.merke = merke
        self.reg_nummer = reg_nummer
        self.km_stand = km_stand

    def kjor(self, km):
        self.km_stand += km # Øker km_stand med argumentet km
    
    def hent_kmstand(self):
        return self.km_stand
    
    def skriv_ut(self):
        print(
            f"Motorsykkelen er av merket {self.merke}, med registreringsnummer {self.reg_nummer}.",
            f"Motorsykkelen har en kilometerstand på {self.km_stand}"
        )