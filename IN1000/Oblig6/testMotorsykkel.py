# Programmet tester at metodene i klassen Motorsykkel virker
# 1.5-8

from motorsykkel import Motorsykkel # Importerer klassen Motorsykkel fra filen motorsykkel.py

def hovedprogram():
    min_motorsykkel = Motorsykkel("Ducati", "AJ65745", 44000) # lagrer objekt i variabel
    min_motorsykkel.skriv_ut() # Kaller på metoden skriv_ut under klassen Motorsykkel (klassen objektet er definert som)
    
    min_andre_motorsykkel = Motorsykkel("Subaru", "EL49211", 120001.2)
    min_andre_motorsykkel.skriv_ut()

    min_tredje_motorsykkel = Motorsykkel("Yamaha", "KL22151", 51054)
    min_tredje_motorsykkel.skriv_ut()

    min_tredje_motorsykkel.kjor(5000)
    print(min_tredje_motorsykkel.hent_kmstand())
    
hovedprogram()