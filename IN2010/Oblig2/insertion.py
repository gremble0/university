import sys

def insertionSort(arr):
    comps = 0
    swaps = 0
    for i in range(1, len(arr)):
        j = i
        while arr[j - 1] > arr[j] and j > 0:
            comps += 2
            temp = arr[j]
            arr[j] = arr[j - 1]
            arr[j - 1] = temp
            j -= 1
            swaps += 1
    return [arr, comps, swaps]

def lesInput():
    arr = []
    
    input_fil = open(sys.argv[1], "r") # krever at filnavnet skrives i terminalen ved kjoering av program
    for linje in input_fil:
        arr.append(int(linje))
    input_fil.close()
    
    output_fil = open(sys.argv[1].split("\\")[-1] + "_insertion.out", "w")
    for element in insertionSort(arr)[0]:
        output_fil.write(str(element) + "\n")
    output_fil.close()

if __name__ == "__main__":
    lesInput()