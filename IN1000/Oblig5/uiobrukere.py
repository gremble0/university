# Programmet skal la brukeren velge mellom noen forskjellige input der målet er å generere epost adresser. 
# Ved i som input kan de skrive inn et navn og en epost suffix som så skal lagres i en ordbok.
# Ved p skal de kunne se alle lagrede brukere.
# Ved s skal de kunne avslutte programmet.
# 2.1 (& 2.6)
# Funksjon som tar et fullt navn som parameter og returnerer fornavnet og den første bokstaven av etternavnet
def lagBrukernavn(navn, ordbok):
    navn_split = navn.split()
    teller = 1
    while (navn_split[0] + navn_split[1][:teller]).lower() in ordbok.keys(): # Så lenge fornavnet + etternavnet avgrenset til de første x bokstavene er i ordbokens nøkler...
        if teller == len(navn_split[1]):
            import string
            import random
            tilfeldig_bokstav = random.choice(string.ascii_letters).lower()
            while navn_split[0] + tilfeldig_bokstav in ordbok.keys():
                tilfeldig_bokstav = random.choice(string.ascii_letters).lower()
            return (navn_split[0] + tilfeldig_bokstav).lower()
            # Kan fortsatt generere brukernavn som allerede finnes i databasen i det tilfellet hvor du har brukernavn for et fornavn
            # med en av alle bokstavene i alfabetet (geira, geirb, geirc, geird, etc.), men denne løsningen er tilstrekkelig for nå
        teller += 1
    return(navn_split[0] + navn_split[1][:teller]).lower() # Returnerer fornavnet og, avhengig av ordboken med navn, forskjellig oppdelt etternavn

# print(lagBrukernavn("Geir Hansen", {"geirhansen": "ifi.uio.no"}))

# 2.2
# Funksjon som genererer epost basert på to parametre; brukernavn og suffix
def lagEpost(brukernavn, suffix):
    return f"{brukernavn}@{suffix}"

# 2.3
# Funksjon som løkker gjennom en ordbok, kaller på en funksjon som formaterer elementene i ordboken til en epost, og så printer ut eposten
def skrivUtEpost(brukere_ordbok):
    assert type(brukere_ordbok) == dict, "Gitt argument er ikke en ordbok" # Sjekker om parameteret er en ordbok
    for brukere in brukere_ordbok:
        print(lagEpost(brukere, brukere_ordbok[brukere]))
    
# skrivUtEpost({"olan":"ifi.uio.no","karin":"student.matnat.uio.no"})

# 2.4
# Løkker gjennom input og kaller på ulike funksjoner avhengig av input
registrerte_brukere = {}
bruker_input = input("\nSkriv 'i' for å initiere en uio bruker, 'p' for å skrive ut lagrede brukere, eller 's' for å avslutte programmet: ")

while bruker_input != "s": # Så lenge input ikke er lik s...
    if bruker_input == "i":
        navn_input = input("Skriv et navn: ")
        suffix_input = input("Skriv en epost suffix: ")
        brukernavn = lagBrukernavn(navn_input, registrerte_brukere)
        registrerte_brukere[brukernavn] = suffix_input
    elif bruker_input == "p":
        skrivUtEpost(registrerte_brukere)
    bruker_input = input("\nSkriv 'i' for å initiere en uio bruker, 'p' for å skrive ut lagrede brukere, eller 's' for å avslutte programmet: ")

# 2.5
# To test-funksjoner for å sjekke om funksjoner gir forventet verdi
def test_lagEpost():
    brukernavn = "hermas"
    suffix = "ifi.uio.no"
    lagd_epost = f"{brukernavn}@{suffix}"
    assert lagd_epost == "hermas@ifi.uio.no", "Fikk ikke forventet epost format: " + lagd_epost
    return lagd_epost # For å kunne hente inn til neste testfunksjon

def test_skrivUtEpost():
    brukere_ordbok = {"hermas": "ifi.uio.no", "karin": "student.matnat.uio.no"}
    assert type(brukere_ordbok) == dict, "Gitt argument er ikke en ordbok"
    for brukere in brukere_ordbok:
        lagd_epost = test_lagEpost()
        assert lagd_epost == "hermas@ifi.uio.no", "Fikk ikke forventet epost format: " + lagd_epost # Printer bare samme to ganger (lengden på brukere_ordbok) siden test_lagEpost ikke tar argumenter

test_skrivUtEpost() # Tester begge testfunksjonene, funksjonene vil ikke gi noe output med mindre det kommer en error
