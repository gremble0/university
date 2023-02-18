import sys

def gnomeSort(arr):
    if (len(arr) == 1):
        return [arr, 0, 0]
    swaps = 0
    comps = 0
    i = 0
    while i < len(arr):
        # print(arr[i-1])
        comps += 3
        if i == 0:
            i = i + 1
        if arr[i] >= arr[i - 1]:
            i = i + 1
        else:
            arr[i], arr[i-1] = arr[i-1], arr[i]
            i = i - 1
            swaps += 1
    return [arr, comps, swaps]

def lesInput():
    arr = []
    
    input_fil = open(sys.argv[1], "r") # krever at filnavnet skrives i terminalen ved kjoering av program
    for linje in input_fil:
        arr.append(int(linje))
    input_fil.close()
    
    output_fil = open(sys.argv[1].split("\\")[-1] + "_gnome.out", "w")
    for element in gnomeSort(arr)[0]:
        output_fil.write(str(element) + "\n")
    output_fil.close()

if __name__ == "__main__":
    lesInput()