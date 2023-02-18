# Hele programmet skal ta inn input fra brukeren helt til de skriver inn tallet 0.
# Så skal tallene gitt som input sjekkes for hvilket som er minst, hvilket som er størst og hva summen av alle tall i input er.
# 2.1-2
# Programmet skal ta input fra bruker og repetere input frem til input == 0
mitt_tall = int(input("Skriv inn et tall: ")) # vil komme error hvis input er tom eller ikke et tall
min_liste = []

while mitt_tall != 0: # Repeterer løkken så lenge mitt_tall ikke er lik 0
    min_liste.append(mitt_tall) # Legger tall inn i liste
    mitt_tall = int(input("Prøv igjen: "))

# 2.3-4
# Programmet skal løkke gjennom listen definert i forrige del av oppgaven.
# Løkken skal så printe ut hvert element og summere tallene.
min_sum = 0
for teller, tall in enumerate(min_liste): # enumerate() lar meg bruke en teller variabel i tillegg til variabel for hvert element av listen min_liste
    print(f"Tall #{teller + 1}: {tall}") # teller + 1 for brukervennlighet siden den vanligvis starter på 0
    min_sum += tall

print(f"Summen av alle tallene: {min_sum}")

# 2.5
# Programmet skal finne det største og minste tallet fra listen basert på brukerinput definert tidligere

# Størst tall:
stoerst = 0
for tall in min_liste:
    if tall > stoerst: # Hvis gjeldende element i min_liste er større enn det som er lagret i stoerst variabelen...
        stoerst = tall # Endre stoerst variabelen til å holde verdien i gjeldende element av min_liste listen

print(f"Største av tallene: {stoerst}")

# Minst tall:
minst = 0
for tall in min_liste:
    if tall < stoerst:
        minst = tall

print(f"Minste av tallene: {minst}")