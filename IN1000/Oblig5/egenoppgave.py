# 5.1
# Skriv et beregningsprogram for skreddere med en
# funksjon som leser inn en fil (som du lager selv og leverer sammen med de andre
# filene) der hver linje beskriver et navn på et mål og selve målet i tommer. Formatet vil
# se slik ut:
# Skulderbredde 4
# Halsvidde 3.2
# Livvidde 10
# Hint: du kan bruke funksjonen .split() for å gjøre dette.
# La programmet legge disse målene i en ordbok med navn på målet som nøkkelverdi
# og returner ordboken. Lag deretter en prosedyre som tar imot en liste av mål og
# benytter seg av tommerTilCm som du skrev tidligere for å skrive ut målene i
# centimeter

# 5.2
# Program som gitt en tekstfil med mål leser inn fra filen, lager en ordbok, konverterer innholdet til
# cm og printer det ut

import sys
def fil_til_ordbok(filnavn):
    ordbok = {}
    fil = open(f"{sys.path[0]}\\{filnavn}", "r") # Åpner fil
    for linje in fil:
        linje = linje.strip("\n")
        linje_split = linje.split()
        ordbok[linje_split[0]] = linje_split[1] # Lager en ny linje i ordboken med nøkkelverdi linje_split[0] (Hva som måles)
                                                # og innholdsverdi linje_split[1] (målet i tommer)
    return ordbok

def skriv_ut_fra_ordbok(ordbok):
    for nokler in ordbok:
        print(f"{nokler}: {tommer_til_cm(ordbok[nokler])}")

def tommer_til_cm(tommer):
    return float(tommer) * 2.54 # Returnerer parameteret castet til float ganger 2.54

skriv_ut_fra_ordbok(fil_til_ordbok("skredder.txt")) 
# Kaller på funksjonen skriv_ut_fra_ordbok med parameteret hentet fra returverdien til fil_til_ordbok med parameteret "skredder.txt"