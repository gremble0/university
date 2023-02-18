# Hele programmet skal ta inn en setning fra brukeren og så registrere alle forskjellige ord og telle deres forekomster og antall bokstaver i hvert ord.
# 4.1
# Funksjonen skal ta ett ord som et argument og returnere antall bokstaver i ordet som 
def antall_bokstaver(ord):
    # return len(ord) Usikker på om dette hadde blitt godkjent siden den bruker en innebygd funksjon
    bokstaver = 0
    for _ in ord: # Trenger ikke løkke-variablen, kan gi error hvis parameter ikke er en streng (avhengig av datatype)
        bokstaver += 1
    return bokstaver

# 4.2
# Funksjonen skal ta inn et parameter (streng) og legge alle enkelte ord inn i en ordbok, så telle hvor mange ganger ordene repeteres
def tell_og_lag_ordbok(setning):
    setning = setning.split() # Error hvis parameter ikke er en streng
    setning_ordbok = {}
    for ord in setning:
        try: # Hvis koden inni utføres uten error (i praksis om ordet allerede finnes i ordboken siden += 1 gir error hvis ordet ikke er i ordboken allerede)
            setning_ordbok[ord] += 1
        except KeyError: # Hvis ordet ikke er i ordboken fra før
            setning_ordbok[ord] = 1
    return setning_ordbok

# 4.3
# Programmet skal ta inn input og behandle input med hjelp av funksjonene definert tidligere
input_setning = input("Skriv en setning: ")
print(f"Antall bokstaver i din setning: {antall_bokstaver(input_setning)}")
input_ordbok = tell_og_lag_ordbok(input_setning)
for ord in input_ordbok:
    print(f"'{ord}' forekommer {input_ordbok.get(ord)} gang(er), og har {antall_bokstaver(ord)} bokstaver")