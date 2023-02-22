# Programmet skal gitt 2 filer med temperaturmålinger, sammenligne temperaturene med hverandre og returnere en
# ordbok med de høyest målte temperaturene for hver individuell måned.
# 4.1
# Definerer funksjon som tar inn en fil, leser gjennom den og genererer ordbok for alle linjene.
# Programmet antar at filen er formatert med komma separerte verdier
import sys

def fil_til_ordbok(filnavn):
    ordbok = {}
    fil = open(f"{sys.path[0]}\\{filnavn}", "r") # Error i windows uten eksplisitt navngivelse av path til fil, funker uten i linux (og macOS?)
    for linje in fil: # Løkker gjennom linjer i fil
        linje = linje.strip("\n") # Fjerner linjeskift fra strengen
        linje_split = linje.split(",") # Splitter strengen ved komma
        ordbok[linje_split[0]] = float(linje_split[1])
    fil.close()
    return ordbok

print(fil_til_ordbok("max_temperatures_per_month.csv"))

# 4.2-3
# Funksjon som gitt 2 argumenter, en ordbok og et filnavn, løkker gjennom argumentene og finner den høyeste temperaturen.
def finn_varmerekord(ordbok, filnavn):
    fil = open(f"{sys.path[0]}\\{filnavn}", "r")
    for linje in fil:
        linje = linje.strip("\n")
        linje_split = linje.split(",")
        gjeldende_temp = float(linje_split[2])
        if gjeldende_temp > ordbok[linje_split[0]]: # Kan bruke den ene filens gjeldende måned til å sjekke verdier i ordboken (hentet fra den andre filen)
            # print(f"Ny varmerekord på {linje_split[1]}. {linje_split[0]}: {gjeldende_temp} grader Celsius (gammel varmerekord var {ordbok[linje_split[0]]} grader Celsius)")
            # Vet ikke om du vil jeg skal slette print delen i oppgave 4.3 så lar den stå kommentert ut
            ordbok[linje_split[0]] = gjeldende_temp
    return ordbok

print(finn_varmerekord(fil_til_ordbok("max_temperatures_per_month.csv"), "max_daily_temperature_2018.csv"))