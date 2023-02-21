# Programmet tester at metodene i klassen Hund virker
# 3.5 

from hund import Hund

def hovedprogram():
    min_hund = Hund(8, 20)
    
    for x in range(4): # Løkker gjennom 4 ganger
        min_hund.spring()
        print(min_hund.vekt, min_hund.metthet) # Printer også metthet for oversiktlighet
        min_hund.spis(2)
        print(min_hund.vekt, min_hund.metthet)

hovedprogram()