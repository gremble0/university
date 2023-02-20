# 5.1
# Med bruk av lister eller ordbøker, lag en quiz med spørsmål og svar.
# 5.2
# programmet skal lage en ordbok med all informasjon om spørsmålene til quizzen.
# hoveddelen av programmet skal gå gjennom en for løkke som skal printe ut spørsmålene og en spesiell melding etter siste spørsmål.
spoersmaal = {
    1: {
        "spoersmaal": "Hva er hovedstaden i Marokko? ",
        "svar": "rabat" # lagrer svar i små bokstaver for å kunne enklere sammenligne med input
        },
    2: {
        "spoersmaal": "Hva heter det høyeste fjellet i USA? ",
        "svar": "denali"
    },
    3: {
        "spoersmaal": "Hva er kvadratroten av 49? ",
        "svar": "7" # lagrer som en streng for å slippe å måtte lage unntak for svar med tall
        },
    4: {
        "spoersmaal": "Befolkningen i India er over 1 milliard (sant/usant) ",
        "svar": "sant"
    },
    5: {
        "spoersmaal": "I hvilket land ligger Java? ",
        "svar": "indonesia"
    }
}

rette_svar = 0
for x in spoersmaal: # går gjennom løkken like mange ganger som det er elementer i spoersmaal variabelen
    bruker_svar = input(spoersmaal[x]["spoersmaal"]) # printer ut det x'te elementets tilknyttede spørsmål fra ordboken og lagrer input fra bruker i en variabel

    if bruker_svar.lower() == spoersmaal[x]["svar"]: # hvis brukerens input er det samme som det x'te elementets tilknyttede svar fra ordboken...
        rette_svar += 1 # legg til +1 til rette_svar variabelen
        print("Riktig. Du har {} rette svar!".format(rette_svar))
    else: # hvis if statementen evaluerer til false...
        print("Feil. Du har {} rette svar.".format(rette_svar))
    
    if x == len(spoersmaal): # hvis x er det samme som lengden på ordboken, altså om vi er på siste spørsmål i ordboken
        print("Quizzen er over og du endte opp med {} rette svar. Du svarte riktig på {}% av spørsmålene!".format(rette_svar, (rette_svar/len(spoersmaal))*100))




