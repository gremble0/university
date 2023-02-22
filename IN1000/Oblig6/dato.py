# Programmet definererer en klasse for datoer med metoder for å: initiere instansvariabler, hente objektets år,
# Formatere datoene i formatet "år-måned-dato", sjekker om en input dag er den samme dagen som i objektet,
# Sjekker om en dato er før eller etter en annen dato, og endrer datoen en dag fremover med hensyn til 
# mulige komplikasjoner
# 4.1-2

class Dato:
    def __init__(self, nytt_aar, ny_maaned, ny_dag):
        self.aar = int(nytt_aar)     # Caster til int siden oppgaven eksplisitt nevner variabeltypen heltall
        self.maaned = int(ny_maaned) # (Umiddelbar error ved gal input, men da hadde det kommet error i kalkulasjoner senere uansett)
        self.dag = int(ny_dag)       
    
    def les_aar(self): # Trenger bare self som parameter siden konstruktoeren har initiert aar som en instansvariabel
        return self.aar # Returnerer aaret til objektet
    
    def formater_dato(self):
        return f"{self.aar}-{self.maaned}-{self.dag}"
    
    def er_dag(self, sjekk_dag):
        if sjekk_dag == self.dag: # Hvis dagen gitt som parameter er det samme som instansvariabelen dag...
            return True
        return False # (else)
    
    def dato_for_etter(self, ny_dato): # Tar inn parameter for ny_dato (forventet variabeltype av samme klasse som self)
        # Formaterer i strenger for senere bruk
        ny_etter = f"{ny_dato.formater_dato()}(Ny dato) kommer etter {self.formater_dato()}(Gammel dato)"
        ny_foer = f"{ny_dato.formater_dato()}(Ny dato) kommer før {self.formater_dato()}(Gammel dato)"
        samme_dato = f"{ny_dato.formater_dato()}(Ny dato) er den samme som {self.formater_dato()}(Gammel dato)"

        if ny_dato.aar > self.aar:
            return ny_etter
        elif ny_dato.aar == self.aar:
            if ny_dato.maaned > self.maaned:
                return ny_etter
            elif ny_dato.maaned == self.maaned:
                if ny_dato.dag > self.dag:
                    return ny_etter
                elif ny_dato.dag == self.dag:
                    return samme_dato
        return ny_foer
    
    def neste_dag(self):
        # Metoden tar hensyn til alle mulige komplikasjoner jeg kunne komme på; skuddår, årskifte og månedskifte
        maaneder = {
            1: 31, 3: 31, 4: 30, 5: 31, 6: 30, 7: 31, 8: 31, 9: 30, 10: 31, 11: 30, 12: 31
            # Ordbok for antall dager i hver maaned. Januar: 31 dager, februar: udefinert (defineres senere), mars: 30 dager, etc.
        }
        if self.aar % 4 == 0: # Setter antall dager i februar avhengig av om det er skuddaar eller ikke
            maaneder[2] = 29
        else:
            maaneder[2] = 28

        if self.dag == maaneder[self.maaned]: # Hvis dagen er lik innholdsverdien til maaneder indeksert ved objektets maaned...
            self.dag = 1
            if self.maaned == 12:
                self.maaned = 1
                self.aar += 1
            else:
                self.maaned += 1
        else:
            self.dag += 1
        return f"{self.formater_dato()}"