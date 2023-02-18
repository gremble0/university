# Basert på logikken i dette bildet https://i.nuuls.com/VOEli.png
# Hvis du har en rest etter å dele et tall fra titallsystemet på 2 kan du sette inn 1 som binær, ellers sett inn 0
# Repeter deling til du kommer til 2 eller 1.
# F.eks:
# 55  |  1   (55/2=27.5 -> desimaltall betyr at det er en rest)
# 27  |  1   (27/2=13.5)
# 13  |  1   (13/2=6.5)
# 6   |  0   (6/2=3)
# 3   |  1   (3/2=1.5)
# 1   |  1   (1/2=0.5)
# 55 -> 110111

import math

input_num = input("Skriv et tall fra titallssystemet: ")

def konverter(desimal):
    binaer = "" # streng fordi vanlig += regner tall i titallsystem
    while desimal >= 1:
        if str(desimal / 2)[-2:] == ".5":
            binaer += "1"
            print(f"{desimal}  -  1  ({desimal}/2 = {desimal/2})")
        else:
            binaer += "0"
            print(f"{desimal}  -  0  ({desimal}/2 = {desimal/2})")
        desimal = math.floor(desimal / 2)
    return binaer[::-1] # reverserer strengen

print(konverter(int(input_num)))