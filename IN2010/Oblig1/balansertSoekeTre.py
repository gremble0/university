import sys
import math

def balansertSoeketre(sortertArr):
    if len(sortertArr) < 1:
        return
    
    median = math.floor(len(sortertArr)/2)
    print(sortertArr[median])
    
    balansertSoeketre(sortertArr[median+1:])
    balansertSoeketre(sortertArr[:median])

def lesInput():
    inp = []

    for linje in sys.stdin:
        inp.append(linje)

    balansertSoeketre(inp)

lesInput()