from collections import defaultdict

class Skuespiller:
    def __init__(self, nmID, navn):
        self.nmID = nmID
        self.navn = navn
        self.filmer = []    

class Film:
    def __init__(self, ttID, tittel, rating):
        self.ttID = ttID
        self.tittel = tittel
        self.rating = rating
    
class Graf(object):
    def __init__(self, noder=list):
        self.noder = noder
        self.graf = []
        self.kanter = []
    
    def hentSkID(self, ttID):
        for _, _, film in self.kanter:
            if ttID == film.ttID:
                return film
        print("Finnes ingen film med denne ID")
    
    def hentFilmID(self, nmID):
        for sk in self.noder:
            if nmID == sk.nmID:
                return sk
        print("Finnes ingen skuespiller med denne ID")
    
    def hentNabo(self, node):
        naboer = {}
        for s, d, m in self.graf:
            if s == node:
                naboer[d] = m
        return naboer
    
    def hentSkNabo(self, node):
        naboer = set()
        for s, d, m in self.graf:
            if s == node:
                naboer.add(d)
        return naboer

# Oppgave 1

def byggGraf():
    filmer = []
    skuespillere = []
    with open("marvel_movies.tsv", encoding = "mbcs") as f: # Virker bare paa marvel filer :(
        for linje in f:
            deler = linje.strip("\n").split("\t")
            film = Film(deler[0], deler[1], deler[2])
            filmer.append(film)

                
    with open("marvel_actors.tsv", encoding = "mbcs") as f:
        for linje in f:
            deler = linje.strip("\n").split("\t")
            sk = Skuespiller(deler[0], deler[1])
            skuespillere.append(sk)
            for ttID in deler[2:]:
                sk.filmer.append(ttID)

    graf = Graf(skuespillere) 

    for sk1 in skuespillere:
        for sk2 in skuespillere:
            if (sk1 != sk2) and set(sk1.filmer).intersection(set(sk2.filmer)):
                graf.graf.append([sk1, sk2, film])

    return [skuespillere, filmer, graf]

# Oppgave 2

def skrivEndeligSti(endeligSti):
    print(endeligSti[0][0].navn)
    i = 1
    while i < len(endeligSti):
        print('=== [', endeligSti[i][0].tittel, '(', endeligSti[i][0].rating, ') ] ===> ', endeligSti[i][1].navn)
        i += 1
    return

def finnKortesteSti(graf, start, slutt):
    if start == slutt:
        print("Samme skuespiller")
        return []
    koe = [[start]]
    besokt = set()

    while koe:
        sti = koe.pop(0)
        gjeldendeNode = sti[-1]

        if gjeldendeNode in besokt:
            continue

        naboer = graf.hentNabo(gjeldendeNode) 

        for nabo in naboer:
            nySti = list(sti)
            nySti.append(nabo)
            koe.append(nySti)

            if nabo == slutt:
                endeligSti = [[start]]
                i = 0
                while i < len(nySti)-1:
                    for s, d, movie in graf.graf:
                        if nySti[i] == s and nySti[i+1] == d:
                            endeligSti.append([movie, d])
                    i += 1
                skrivEndeligSti(endeligSti)
                return endeligSti  
        besokt.add(gjeldendeNode)

    print("Ingen sti mellom", start.navn, 'og', slutt.navn, "\n")
    return

# Oppgave 3

def chillestVei(graf, start, slutt):
    ubesoekteNoder = graf.noder[:]
    dist = {}
    visited = {}
    
    for node in ubesoekteNoder:
        dist[node] = float("inf")
    dist[start] = 0
    
    while ubesoekteNoder: # fortsetter saa lenge ubesoekte noder ikke er tom
        minst = None
        for node in ubesoekteNoder:
            if minst == None:
                minst = node
            elif dist[node] < dist[minst]:
                minst = node
        
        naboer = graf.hentNabo(minst)
        for nabo in naboer:
            temp = dist[minst] + (10 - float(naboer[nabo].rating))
            if temp < dist[nabo]:
                dist[nabo] = temp
                visited[nabo] = minst
        ubesoekteNoder.remove(minst)
    
    sti = []
    node = slutt
    while node != start:
        sti.append(node)
        node = visited[node]
    sti.append(start)
    sti.reverse()
    
    endeligSti = [[start]]
    i = 0
    vekt = 0
    while i < len(sti)-1:
        for s, d, film in graf.graf:
            if sti[i] == s and sti[i + 1] == d:
                endeligSti.append([film, d])
                vekt += (10 - float(film.rating))
        i += 1

    skrivEndeligSti(endeligSti)
    print('Total weight:', vekt)
    return dist

# Oppgave 4

def finnAlleKomponenter(graf):
    grafDict = defaultdict(set)
    for skuespiller in graf.noder:
        grafDict[skuespiller] = graf.hentSkNabo(skuespiller)
    
    visited = set()
    resultat = []
    for node in grafDict:
        if node not in visited:
            connected, visited = finnKomponenter(node, visited, grafDict)
            resultat.append(connected)
    return resultat

def finnKomponenter(node, visited, grafDict):
        resultat = []
        noder = set([node])
        while noder:
            node = noder.pop()
            visited.add(node)
            noder = noder or grafDict[node] - visited
            resultat.append(node)
        return resultat, visited

def kompStoerelse(graf):
    komponenter = finnAlleKomponenter(graf)
    teller = []
    for komp in komponenter:
        teller.append(len(komp))
    
    temp = dict((i, teller.count(i)) for i in teller)
    for i in temp:
        print('There are', temp[i], 'components of size', i)



def main():
    print("\n----------- OPPGAVE 1 -----------n")

    skuespillere, filmer, graf = byggGraf()

    print("Noder: ", len(skuespillere))
    print("Kanter: ", len(graf.graf))
    
    print("\n----------- OPPGAVE 2 -----------n")
    finnKortesteSti(graf, skuespillere[5], skuespillere[2])

    print("\n----------- OPPGAVE 3 -----------n")
    chillestVei(graf, skuespillere[6], skuespillere[2])

    print("\n----------- OPPGAVE 4 -----------n")
    kompStoerelse(graf)
    print('\n')

if __name__ == "__main__":
    main()