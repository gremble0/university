import random
import math

# 4a
class Onske:
def __init__(self, beskrivelse, antall, minimumspris):
    self._beskrivelse = beskrivelse
    self._antall = antall
    self._minimumspris = minimumspris
    
def passer(self, maksimumspris):
    return self._antall > 0 and self._minimumspris <= maksimumspris

def valgt(self):
    self._antall -= 1
    return self._beskrivelse

def hent_beskrivelse(self):
    return self._beskrivelse

def __str__(self):
    return f"Onskets beskrivelse: {self._beskrivelse}\nOnskets minimumspris: {self._minimumspris}"

# 4b
class Onskeliste:
def __init__(self):
    self._onskeliste = []

def hent_onskeliste(self):
    return self._onskeliste

def nytt_onske(self, beskrivelse, antall, minimumspris):
    onske = Onske(beskrivelse, antall, minimumspris)
    self._onskeliste.append(onske)

def hent_onsker(self, maksimumspris):
    passer_onsker = []
    for onsker in self._onskeliste:
        if onsker.passer(maksimumspris):
            passer_onsker.append(str(onsker))
        else:
            passer_onsker("Ikke valgbart onske")
    return passer_onsker

def onske_oppfylles(self, valgt_onske):
    self._onskeliste[valgt_onske].valgt()
    return self._onskeliste[valgt_onske].hent_beskrivelse() 
    # kunne brukt str(), men den returnerer også minimumspris, noe som ikke står som del av ønsket returverdi i oppgave 4b

# 4c
class Gave:
def __init__(self, beskrivelse, giver):
    self._beskrivelse = beskrivelse
    self._giver = giver

def hent_info(self):
    return f"Gaven er: {self._beskrivelse}\nGaven er fra: {self._giver}"

# 4d
class Juleferiekalender:
def __init__(self, dager):
    self._kalender = {}
    # løkker gjennom antall dager spesisert i parameter og lagrer None verdier i assosierte nøkler
    for x in range(25, 25 + dager):
        if x > 31:
            self._kalender[x-31] = None
        else:
            self._kalender[x] = None

def ny_gave(self, beskrivelse, giver, dag):
    self._kalender[dag] = Gave(beskrivelse, giver)

def hent_dagens_gave(self, dag):
    try:
        if dag < 25:
            maaned = "januar"
        else:
            maaned = "desember"
        return f"{dag}. {maaned}: {self._kalender[dag].hent_info()}"
    except:
        # Kommer error ved hent_info metoden når det ikke finnes en gave på den dagen siden den prøver 
        # å kalle på hent_info metoden på None. Dette kan vi bruke for å filtrere ut dager uten gaver
        return "Fant ingen gaver denne dagen"

def hent_ant_dager(self):
    return len(self._kalender)

# 4e
class Julegavefikser:
def __init__(self, dager):
    self._juleferiekalender = Juleferiekalender(dager)
    self._onskeliste = Onskeliste()
    self._neste_dag = 25

def les_onsker_fra_fil(self, filnavn):
    fil = open(filnavn, "r")
    for linje in fil:
        linje_split = linje.strip().split(";")
        self._onskeliste.append(Onske(linje_split[0], linje_split[1], linje_split[2]))
        # linje_split[0] = beskrivelse, linje_split[1] = antall, etc

# 4f
def velg_gave(self):
    giver = input("Skriv ditt navn: ")
    maksimumspris = int(input("Skriv inn din maksimumspris: "))
    passer_onsker = self._onskeliste.hent_onsker(maksimumspris)
    for teller, onsker in enumerate(passer_onsker):
        print(teller+1, onsker) # +1 for å ikke starte på 0 for menneskelig preferanse

    input_onske = int(input("Skriv tallet på ønsket du ønsker å oppfylle: "))
    onskeliste = self._onskeliste.hent_onskeliste()
    if onskeliste[input_onske-1]:
        beskrivelse = onskeliste[input_onske-1].valgt()
    
    input_dag = int(input("Skriv hvilken dag du ønsker å legge gaven i: "))
    self._juleferiekalender.ny_gave(beskrivelse, giver, input_dag)

def ny_dag(self):
    gave_i_dag = self._juleferiekalender.hent_dagens_gave(self._neste_dag)
    if self._neste_dag == 31:
        self._neste_dag = 1
    else:
        self._neste_dag += 1
    return gave_i_dag

julegavefikser = Julegavefikser(9)
print(julegavefikser.ny_dag())
julegavefikser._onskeliste.nytt_onske("En klem", 4, 0)
print(julegavefikser._onskeliste._onskeliste[0]._beskrivelse)
julegavefikser.velg_gave()
for x in range(3,28):
print(julegavefikser._juleferiekalender.hent_dagens_gave(x))




    















































# def beOmNavn(navneliste):
#     input_navn = input("Skriv et navn: ")
#     while input_navn not in navneliste:
#         input_navn = input("Skriv et navn: ")
#     return input_navn

# def lagSynonymordbok(listeAvLister):
#     ordbok = {}
#     for lister in listeAvLister:
#         for innhold in lister:
#             try:
#                 ordbok[lister[0]] += [innhold]
#             except KeyError:
#                 ordbok[lister[0]] = [innhold]

#     print(ordbok)

# lagSynonymordbok([["Jeg","Deg"], ["Jon"], ["Blad"]])