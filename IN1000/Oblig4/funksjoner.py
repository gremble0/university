# 1.1
# Programmet skal bestå av en funksjon med to parametre som skal returneres summert.
def adder(tall1, tall2):
    return f"{tall1} + {tall2} = {tall1 + tall2}" # formaterer tekst og returnerer strengen

print(adder(2, 4)) # kaller funksjon og printer ut
print(adder(22, 51))

# 1.2-3
# Programmet skal bestå av en funksjon som skal ta to paremtre basert på input. 
# Det første parametret vil være en lengre streng, og det andre parametret en enkel bokstav.
# Så skal antall forekomster av bokstaven i den lengre setningen telles og returneres
def tell_bokstaver(setning, bokstav):
    bokstav = bokstav[0] # i tilfelle bruker skriver inn flere bokstaver forkorter jeg variabelen til den første bokstaven i strengen siden det kun spørres etter en enkelt bokstav
    forekomster = 0 # initierer tom teller variabel for antall forekomster av bokstav i setning
    for bokstaver in setning: # løkker gjennom alle bokstaver i setning variabelen
        if bokstaver == bokstav:
            forekomster += 1
    return f"I setningen '{setning}' er det {forekomster} forekomster av bokstaven '{bokstav}'"

print(tell_bokstaver(input("Skriv inn et eller flere ord: "), input("Skriv inn en bokstav: "))) # kaller funksjon og printer ut