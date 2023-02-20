# Oppgave 3
# 3.1
dato1 = input("Skriv inn en dato (DDMM - 0405 = 4. mai) ")
dato2 = input("Skriv inn en dato til (DDMM - 0405 = 4. mai) ")
# 3.2
def sjekkDatoer(dato1, dato2): # definerer funksjon
    # Følgende måte å lagre datoer på er ikke perfekt siden den vil godta ting som "10fasdfsadf04",  men det funker hvis bruker skriver slik opplyst.
    dag1 = int(dato1[:2])
    dag2 = int(dato2[:2])
    maaned1 = int(dato1[-2:])
    maaned2 = int(dato2[-2:])

    if dato1 != dato2: # Hvis datoene er ulike...
        if maaned2 > maaned1: # Hvis maaned2 er større enn maaned1...
            return "Riktig rekkefølge"
        elif maaned2 == maaned1: 
            if dag2 > dag1: # Hvis maaned2 er lik maaned1 og dag2 er større enn dag1...
                return "Riktig rekkefølge"
            else: # Hvis maaned2 er større enn maaned1, men dag2 ikke er større enn dag1...
                return "Feil rekkefølge"
        else: # Hvis maaned2 ikke er større enn maaned1...
            return "Feil rekkefølge"
    else: # Hvis datoene er like...
        return "Samme dato!"

print(sjekkDatoer(dato1, dato2))
# 3.3
# Oppgavebeskrivelsene er litt vage så tolket 3.1 & 3.2 på måten beskrevet i 3.3 (tror jeg?).