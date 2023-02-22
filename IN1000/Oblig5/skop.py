def minFunksjon():
    for x in range(2):
        c = 2
        print(c)
        c += 1
        b = 10
        b += a
        print(b)
    return(b)

def hovedprogram():
    a = 42
    b = 0
    print(b)
    b = a
    a = minFunksjon()
    print (b)
    print (a)

hovedprogram()

"""
Programmet definerer først 2 prosedyrer uten parametere. Programmet har ingen globale variabler så alle variabler er lokale
for funksjonene de er definert i. Første ting som skjer ved kjøring av programmet er en kalling på hovedprogram() funksjonen.
Funksjonen definerer først 2 variabler, a og b, som får verdiene 42 og 0. Så printer den ut b som så langt fortsatt har verdien 0.
Deretter settes verdien til b lik a og b er nå lik 42. Så settes a lik returverdien av minFunksjon(). minFunksjon's første operasjon
er en for løkke som skal repeteres 2 ganger. I for løkken lages en ny variabel - c som settes lik 2 for hver gjennomgang og printes ut.
Dermed vil det skrives ut 2 i konsollen for både første og andre gjennomgang av løkken. Etter c printes ut legges det til 1 i innholdet
i c, så c vil være lik 3 på slutten av funksjonen (ikke 4 siden c settes tilbake til 2 for andre gjennomgang av løkken). Fortsatt inni 
løkken lages en ny variabel - b som settes lik 10. Så forsøkes det å legge til innholdet av en variabel - a - til innholdet av b, men 
funksjonen minFunksjon kjenner ingen variabel ved navn a, dermed NameError. Hvis vi ignorerer erroren og later som at programmet kjører 
videre med at b fortsatt er lik 10, vil det så printes ut innholdet av b i konsollen (10), for hver gjennomgang av løkken (2 ganger).
Så hadde verdien av minFunksjon's b variabel (10), lagres i hovedprogram funksjonens a variabel. Til slutt printer hovedprogram 
funksjonen ut innholdet av b (42) og a (10) variabelene.
"""