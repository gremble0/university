# 3.1-5
# programmet skal ta inn input for brukerens alder for å kalkulere og printe ut tilsvarende pris
def billettpris_kalk():
    alder = input("Skriv din alder: ")
    try: # forsøk å gjøre det indenterte under 
        alder = int(alder)
        billettpris = 0
        if 0 < alder < 150: # om alderen er mellom 0 og 150...
            if alder <= 17: # om alderen er under 17...
                billettpris = 30
            elif 17 < alder < 63: # hvis alderen er mellom 17 og 63...
                billettpris = 50
            else: # hvis alderen hverken er under 17 eller mellom 17 og 63...
                billettpris = 35
            print("Din bilett vil koste {}kr".format(billettpris))
        else: # hvis alderen ikke er mellom 0 og 150
            print("Jeg tviler på at {} år gamle mennesker eksisterer".format(alder))
    except ValueError: # hvis det kommer en ValueError (i dette tilfellet at bruker skriver noe annet enn et tall når det forsøkes å konverteres fra streng til int)...
        print(alder, "er ikke et tall")
        billettpris_kalk()

billettpris_kalk()

# Hvis du bare hadde sjekket if alder < 17, if alder < 63 og tatt else for hvis alder var større enn 63 hadde programmet fungert på blant annet negative og veldig høye tall