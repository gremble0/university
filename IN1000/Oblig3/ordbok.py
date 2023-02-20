# 2.1
# programmet definerer en ordbok av produkter med tilhørende priser
produkter = {
    "Melk": 14.90,
    "Brød": 24.90,
    "Yoghurt": 12.90,
    "Pizza": 39.90
}
print(produkter)
# 2.2
# programmet henter input om et produkt og
# produkter[input("Skriv inn et produkt: ")] = input("Skriv produktets pris: ") hadde vært veldig praktisk om dette funket, men da ville den spurt deg om prisen før produktet
produkt_input = input("Skriv inn et produkt: ")
pris_input = input("Skriv inn produktets pris i kr: ")
produkter[produkt_input] = float(pris_input) # lager en ny nøkkel fra produkt_input og setter dens verdi lik pris_input castet til en float.
# kommer error hvis bruker skriver noe annet enn et tall som pris, men ingen vits å fikse det ettersom bruker blir eksplisitt spurt om pris i kroner
print(produkter)
