# Programmet tester at metodene i klassen Dato virker
# 4.3

from dato import Dato

# 4.3a
min_dato = Dato(2000, 1, 15)

# 4.3b
print(f"Året er: {min_dato.les_aar()}") # Kaller på objektets les_aar metode

# 4.3c
if min_dato.dag == 15: # Sjekker om objektets instansvaraibel dag er 15
    print("Loenningsdag")
elif min_dato.dag == 1:
    print("Ny maaned, nye muligheter")

# 4.3d
formatert_dato = min_dato.formater_dato()

# 4.4e
print(formatert_dato)

# 4.4f
min_dato.neste_dag()
formatert_dato = min_dato.formater_dato()
print(formatert_dato)

# 4.4g
nytt_aar = int(input("Skriv et aar: ")) # Antar gylding input (tall (og gyldige dager og måneder for følgende input))
ny_maaned = int(input("Skriv en maanede: "))
ny_dag = int(input("Skriv en dag: "))

ny_dato = Dato(nytt_aar, ny_maaned, ny_dag)
print(min_dato.dato_for_etter(ny_dato))