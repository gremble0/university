from gnome import gnomeSort
from insertion import insertionSort
from quick import quickSort
from heap import heapSort
import timeit
import sys
import math

def lesInput():
    arr = []
    
    input_fil = open(sys.argv[1], "r") # krever at filnavnet skrives i terminalen ved kjoering av program
    for linje in input_fil:
        arr.append(int(linje))
    input_fil.close()
    
    output_fil = open(sys.argv[1].split("\\")[-1] + "_results.csv", "w")    
    output_fil.write("n, insert_cmp, insert_swaps, insert time, quick_cmp, quick_swaps, quick_time, gnome_cmp, gnome_swaps, gnome_time,heap_cmp, heap_swaps, heap_time \n")

    for x in range (0, len(arr) + 1):
        iStart = timeit.default_timer()
        insertionSorted = insertionSort(arr[:x])
        iEnd = timeit.default_timer() 

        qStart = timeit.default_timer()
        quickSorted = quickSort(arr[:x], 0, math.floor(len(arr[:x]) - 1), 0, 0)
        qEnd = timeit.default_timer() 

        gStart = timeit.default_timer()
        gnomeSorted = gnomeSort(arr[:x])
        gEnd = timeit.default_timer() 
        
        hStart = timeit.default_timer() 
        heapSorted = heapSort(arr[:x])
        hEnd = timeit.default_timer() 

        output_fil.write(("%s, " * 12 + "%s\n") % ( # det er 13 verdier paa hver linje, n + 3 for hver algoritme * 4 algoritmer
                            str(x), str(insertionSorted[1]), str(insertionSorted[2]), str(math.ceil((iEnd - iStart)*1000000)),
                            str(quickSorted[1]), str(quickSorted[2]), str(math.ceil((qEnd - qStart)*1000000)),
                            str(gnomeSorted[1]), str(gnomeSorted[2]), str(math.ceil((gEnd - gStart)*1000000)),
                            str(heapSorted[1]), str(heapSorted[2]), str(math.ceil((hEnd - hStart)*1000000))
                        ))

    output_fil.close()

if __name__ == "__main__":
    lesInput()