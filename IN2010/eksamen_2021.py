import math

def FindSummands(A, x):
    for i in range(len(A)):
        low = i
        high = len(A) - 1 # n er stoerrelsen paa A
        while low < high:
            if A[i] + A[high] > x:
                high = math.floor(low + high / 2)
            elif A[i] + A[high] < x:
                low = math.floor(low + high / 2)
            else:
                print(A[i], A[high])
                break

FindSummands([0,2,4,6,8,10], 10)


"""
Procedure WhopsOppgjor(G, T):
    while G is not empty:
        v = velg vilkarlig element fra V
        K = tom graf
        K.append(DFSVisit(G, v, [])) // legg til det som ender opp i visited i K
        if SjekkKomponent(K, T):
            G = G - K // fjerner alle elementene i K fra G og fortsetter lokken
            continue
        else:
            return false
    return true // hvis algoritmen kommer seg hit uten a returnere false i while 
            // lokken er oppgjoret mulig
        
Procedure DFSVisit(G, v, visited):
    visited.append(v)
    for (v, u) in E:
        if u not in visited:
            DFSVisit(G, u, visited)	

Procedure SjekkKomponent(K, T):
    // tar en komponent og sjekker om oppgjoret er mulig for den komponenten
    // hvis det ikke er mulig for denne komponenten vet vi at oppgjoret ikke er
    // mulig, hvis det er mulig returner true
    delSum = 0
    for v in V: // nodene til K
        delsum = delsum + T[v]
    return delsum = 0
"""