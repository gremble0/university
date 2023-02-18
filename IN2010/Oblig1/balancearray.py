import sys
import math

def balancearray(sortertArr):
    if len(sortertArr) < 1:
        return
    
    median = math.floor(len(sortertArr)/2)
    print(sortertArr[median])
    
    balancearray(sortertArr[median+1:])
    balancearray(sortertArr[:median])

def lesInput():
    inp = []

    for linje in sys.stdin:
        inp.append(linje)

    balancearray(inp)

lesInput()