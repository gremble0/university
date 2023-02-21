# Programmet skal definere et par regnefunksjoner og basert på input kjøre gjennom funksjonene og printe resultatene til brukeren
# 1.1
# Funksjon som returener summen av to tall
def addisjon(tall1, tall2):
    return tall1 + tall2

# print(addisjon(2, 4))

# 1.2
# Funksjon som returnerer tall1 trukket fra tall2
def subtraksjon(tall1, tall2):
    return tall1 - tall2

assert subtraksjon(3, 2) == 1
assert subtraksjon(-4, -2) == -2
assert subtraksjon(-2, 4) == -6

# Funksjon som dividerer tall1 på tall2
def divisjon(tall1, tall2):
    return tall1 / tall2

assert divisjon(6, 2) == 3
assert divisjon(-10, 5) == -2
assert divisjon(-8, -4) == 2

# 1.3
# Funksjon som sjekker om parameter er positivt og returnerer parameter
def tommerTilCm(antallTommer):
    assert antallTommer > 0
    return antallTommer * 2.54

# print(tommerTilCm(5))

# 1.4
def skrivBeregninger():
    tall1, tall2 = float(input("Skriv et tall: ")), float(input("Skriv et tall til: ")) # Definerer to variabler fra input
    print(f"\n{tall1} + {tall2} = {addisjon(tall1, tall2)}")
    print(f"{tall1} - {tall2} = {subtraksjon(tall1, tall2)}")
    print(f"{tall1} / {tall2} = {divisjon(tall1, tall2)}")
    
    tommerInput = float(input("\nSkriv en lengde i tommer: "))
    print(f'{tommerInput}" er {tommerTilCm(tommerInput)}cm')

skrivBeregninger()