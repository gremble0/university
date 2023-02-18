# 5.1
# Lag et program som lagrer bursdager og som lar brukeren legge til nye og endre de lagrede bursdagene
# 5.2
# Programmet skal ta inn input for navn og bursdag og legge inputene inn i en ordbok. Dette skal løkkes til brukeren melder seg ut av løkken.
bursdager = {
    "Herman": "19. November"
}

def endre_bursdag(navn, bursdag):
    print("\n") # Lager ny linje for oversiktlighet
    bursdager[navn] = bursdag # Setter bursdager ordbokens verdi lik bursdag inputet der nøkkelen er navn inputet
    for navn in bursdager: # Løkker gjennom bursdager ordboken like mange ganger som den har elementer i seg
        print(f"{navn} har bursdag {bursdager[navn]}")

    input_navn = input("Skriv navnet til den du vil endre/legge til bursdagen på, eller 'q' for å avslutte : ")
    if input_navn != "q": # Hvis input_navn ikke er lik q, fortsett programmet
        input_bursdag = input("Skriv når vedkommende har bursdag, eller 'q' for å avslutte: ")
        if input_bursdag != "q": # Hvis input_bursdag ikke er lik q, fortsett programmet
            endre_bursdag(input_navn, input_bursdag)


endre_bursdag(input("Skriv navnet til den du vil endre/legge til bursdagen på: "), input("Skriv når vedkommende har bursdag: "))

# Programmet sjekker ikke om input faktisk er navn og bursdag, men vil kjøre uten problemer hvis input er noe annet.