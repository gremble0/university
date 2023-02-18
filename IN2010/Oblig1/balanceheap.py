import math
import heapq
import sys

def balanceheap(heap):
    if len(heap) < 1:
        return
    
    heapSort = []
    heapq.heapify(heapSort) # Usikker paa om dette egentlig er nodvendig type(heap) og type(heapSort) er fortsatt bare
                            # list. Ser ikke ut som at heapify gjoer noe annet enn aa sortere en liste/array til aa 
                            # oppfylle krav for aa vaere en heap?
    for _ in range(0, len(heap)):
        heapq.heappush(heapSort, heapq.heappop(heap))
    
    # heapSort er fortsatt egentlig en liste saa kan man ikke like godt gjoere balancearray(heapSort) her?

    median = math.floor(len(heapSort)/2)
    print(heapSort[median])
    
    balanceheap(heapSort[median+1:])
    balanceheap(heapSort[:median])

def lesInput():
    inp = []

    for linje in sys.stdin:
        inp.append(linje)

    balanceheap(inp)

lesInput()