# Hele programmet skal ta input fra brukeren og legge input inn i en nøstet liste. Den nøstede listen skal så indekseres av brukeren for å få ut forskjellige elementer fra dem.
# 3.1-2
# Programmet skal bestå av noen tomme lister og en løkke som skal løkke gjennom input og legge input inn i listene.
steder = []
klesplagg = []
avreisedatoer = []

for x in range(2): # unødvendig mange inputs krevd hvis range var 6 slik oppgaven egentlig spurte om
    input_reisemaal = input("Skriv et sted du ønsker å reise til: ")
    steder.append(input_reisemaal)
    input_klesplagg = input("Skriv et klesplagg du vil ta med deg dit: ")
    klesplagg.append(input_klesplagg)
    input_avreisedato = input("Skriv når du har tenkt å reise dit: ")
    avreisedatoer.append(input_avreisedato)

# 3.3
# Lager en liste som består av de andre tre listene definert tidligere
reiseplan = [steder, klesplagg, avreisedatoer]

# 3.4
# Programmet løkker gjennom alle elementer i reiseplan listen og printer dem ut
for reiser in reiseplan:
    print(reiser)

# 3.5
# Programmet skal hente inn input for å la brukeren indeksere den nøstede listen og hente ut ønsket data
print(reiseplan) # For å la brukeren se hva de kan velge mellom
input_liste_indeks1 = int(input("Skriv en indeks for å velge en av listene ovenfor (0-2): ")) # Error hvis input ikke er et tall
if input_liste_indeks1 in range(0, 3): # Hvis input_liste_indeks1 er mellom 0 til 2
    print(reiseplan[input_liste_indeks1]) # Brukervennlighet
    input_liste_indeks2 = int(input(f"Skriv en indeks for listen ovenfor (0-{len(reiseplan[input_liste_indeks1]) - 1})")) # len(reiseplan[input_liste_indeks1]) gjør programmet mer dynamisk hvis vi hadde endret antall reiser lagret
    if input_liste_indeks2 in range(0, len(reiseplan[input_liste_indeks1])): # Hvis input_liste_indeks2 er mellom 0 og lengden på listen inni reiseplan indeksert med input_liste_indeks1...
        print(reiseplan[input_liste_indeks1][input_liste_indeks2])
    else:
        print("Ugyldig input!")
else:
    print("Ugyldig input!")