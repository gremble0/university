# 6.1
# Skriv en klasse Person med en konstruktør som tar imot navn og alder og oppretter
# og initialiserer instansvariabler med disse. I tillegg skal konstruktøren opprette en
# instansvariabel hobbyer som en tom liste . Skriv en metode leggTilHobby som tar
# imot en tekststreng og legger den til i hobbyer-listen. Skriv også en metode
# skrivHobbyer. Denne metoden skal skrive alle hobbyene etter hverandre på en linje.
# Gi deretter Person-klassen en metode skrivUt som i tillegg til å skrive ut navn og
# alder kaller på metoden skrivHobbyer.
# Lag deretter et testprogram for klassen Person der du lar brukeren skrive inn navn og
# alder, og oppretter et Person-objekt med informasjonen du får. Deretter skal brukeren
# ved hjelp av en løkke få legge til så mange hobbyer de vil. Når brukeren ikke lenger
# ønsker å oppgi hobbyer skal informasjon om brukeren skrives ut.

# Programmet definererer en klasse for personer med metoder for å: initiere instansvariabler
# legge til hobbyer, skrive ut hobbyer og skrive ut informasjon om personen (inkludert alle hobbyer)
# Programmet skal kjøres gjennom en hovedfunksjon som skal definere et objekt fra input. Funksjonen
# skal løkke gjennom input for personens hobbyer frem til brukeren avslutter løkken
# 6.2

class Person:
    def __init__(self, navn, alder):
        self.navn = navn
        self.alder = alder
        self.hobbyer = []

    def legg_til_hobby(self, hobby):
        if hobby == "":
            return # Avslutter metode hvis hobby er tom
        self.hobbyer.append(hobby)
    
    def skriv_hobbyer(self):
        if len(self.hobbyer) == 0:
            print(f"{self.navn} har ingen hobbyer")
            return # Avbryter metode dersom person ikke har noen hobbyer
        print("Hobbyer: ")
        for hobbyer in self.hobbyer:
            print(hobbyer)
        
    def skriv_ut(self):
        print(f"Navn: {self.navn}\nAlder: {self.alder}")
        self.skriv_hobbyer()

def hovedprogram():
    input_navn = input("Skriv et navn: ")
    input_alder = int(input(f"Skriv {input_navn} sin alder: ")) # Antar gyldig input for å ikke få error ved casting
    input_person = Person(input_navn, input_alder) # Oppretter objekt fra input

    input_hobby = input(f"Skriv en av {input_navn} sine hobbyer eller q for å avslutte: ")
    while input_hobby != "q":
        input_person.legg_til_hobby(input_hobby)
        input_hobby = input(f"Skriv en av {input_navn} sine hobbyer eller q for å avslutte: ")
    
    print("\n") # Oversiktlighet
    input_person.skriv_ut()

hovedprogram()