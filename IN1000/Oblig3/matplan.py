# 4.1
# Definerer en ordbok og lagrer den til variabelen "beboere"
beboere = {
    "Kari Nordmann": {
        "Frokost": "Brød",
        "Lunsj": "Egg",
        "Middag": "Pølse"
        },
    "Bjørn Svensson": {
        "Frokost": "Papir",
        "Lunsj": "Gjørme",
        "Middag": "Støv"
    },
    "Geir Lenardt Hansen": {
        "Frokost": "Biller",
        "Lunsj": "Potetgull",
        "Middag": "Taco"
    }
}
# 4.2
# programmet lager en prosedyre som skal skrive ut alle navnene lagret i beboere ordboken, så la brukeren velge mellom disse beboerene for å se matplanen deres.
def matplan():
    print(beboere.keys()) # printer ut alle nøklene i beboere ordboken. Blir ikke like pent fremstilt på denne måten, men det er mye mer dynamisk enn å kalle alle beboerne manuelt.
    beboer_input = input("Skriv navnet til en beboer for å se matplanen deres: ")
    try:
        print("Til frokost spiser {beboer} {frokost}.\nTil lunsj spiser {beboer} {lunsj}.\nTil middag spiser {beboer} {middag}.".format( # endrer det i krøllparantesene i "" til variabler definert under
            beboer = beboer_input,
            frokost = beboere[beboer_input]['Frokost'],
            lunsj = beboere[beboer_input]['Lunsj'],
            middag = beboere[beboer_input]['Middag'])
        )
    except KeyError: # hvis det kommer en KeyError (i dette tilfellet at bruker skriver noe som ikke er en nøkkel i ordboken)...
        print(beboer_input, "er ikke registrert i våre systemer, vennligst velg et navn fra listen.")
        matplan() # sender bruker til starten av prosedyren
matplan()

# 4.3
# a) Liste - Det er kun én data som skal lagres per person, dermed ikke vits å lage ordbok. 
# Det vil ikke være noen duplikate data som burde forkortes, dermed ikke mengde.
# b) Ordbok - Flere antall data som skal lagre per person, dermed er hverken liste eller mengde praktisk
# c) Liste - Kun navn som skal lagres for hvert lottoresultat, dermed ikke vits å lage ordbok.
# Usannsynlig at samme person vinner flere ganger på rad, og i dette tilfellet er det nok heller ikke ønskelig å fjerne duplikate seiere, dermed ikke mengde
# d) Mengde - Kun interessant å vite om det enten er noen allergier eller ikke for å planlegge menyen, dermed ikke liste.
# Kunne for så vidt vært greit å vite hvor mange av en allergi det er i forhold til hvor store mengder som burde forberedes av allergivennlig mat, så ordbok kunne vært smart, men jeg tror mengder er svaret dere er ute etter.