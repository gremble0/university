# Oppgave 4:
# Programmet lager en regneklynge med gitt antall maks racks, lager noder
# i løkker og legger dem til i regneklyngen med hjelp av regneklyngens 
# settInnNode metode. Til slutt printer programmet ut informasjonen for å 
# sjekke at metodene gir forventet resultat.

from regneklynge import Regneklynge
from node import Node
from datasenter import Datasenter

def hovedprogram():
    abel = Regneklynge(12)

    for _ in range(650):
        # lager 650 noder med 64gb minne og 1 prosessor og legger dem inn i regneklyngen
        ny_node = Node(64, 1)
        abel.settInnNode(ny_node)

    for _ in range(16):
        ny_node = Node(1024, 2)
        abel.settInnNode(ny_node)
        
    print(f"Antall prosessorer: {abel.antProsessorer()}")
    print(f"Antall rack: {abel.antRacks()}")
    print(f"Noder med minst 32GB minne: {abel.noderMedNokMinne(32)}")
    print(f"Noder med minst 64GB minne: {abel.noderMedNokMinne(64)}")
    print(f"Noder med minst 128GB minne: {abel.noderMedNokMinne(128)}")

    print() # linjeskift

    # Oppgave 5:
    # Leser inn fra de to filene "saga.txt" og "abel.txt" med bruk av datasenter
    # klassens les_fra_fil metode. Skriver så ut informasjon om datasenterets regneklynger.

    datasenter = Datasenter()
    datasenter.les_fra_fil("abel.txt")
    datasenter.les_fra_fil("saga.txt")
    datasenter.skrivUtAlt()

# Kjører hovedprogram så lenge programmet kjøres direkte fra denne filen
if __name__ == "__main__":
    hovedprogram()