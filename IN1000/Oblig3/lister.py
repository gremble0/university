# 1.1
# programmet skal lage en liste, legge et tall inn i listen og printe ut det første og det tredje elementet i listen
tall = [1, 4, 10]
tall.append(22) # legger til tallet 22 bakerst i listen
print(tall[0], tall[2])
# 1.2
# programmet løkker gjennom 4 bruker input og legger hvert av inputene inn som et element i en tom liste
liste_navn = []
for x in range(4): # løkker gjennom 4 ganger
    liste_navn.append(input("Skriv et navn: ")) # legger input til i liste
# 1.3
# programmet gir ulik tilbakemelding avhengig av om strengen "Herman" er et element i listen definert tidligere
if "Herman" in liste_navn: # hvis "Herman" er et element i listen...
    print("Du husket meg!")
else: 
    print("Glemte du meg?")
# 1.4
# programmet multipliserer og summerer alt i listen "tall", legger resultatene i en liste, legger listen med resultatene og de initielle tallene i en ny liste, og fjerner de to siste tallene fra den siste listen
sum = 0 # lager tom tallvariabel for senere bruk
for x in range(len(tall)): # løkker gjennom like mange ganger som det er elementer i listen "tall"
    sum += tall[x]
produkt = 1 # 1 i stedet for 0 siden den skal ganges med
for x in range(len(tall)):
    produkt *= tall[x]
flere_tall = [sum, produkt] # lager ny liste med sum og produkt som elementer
enda_flere_tall = tall + flere_tall # lager enda en ny liste med alle elementene fra både tall og flere_tall listene
enda_flere_tall.pop() # fjerner siste element fra liste
enda_flere_tall.pop()
print(enda_flere_tall)