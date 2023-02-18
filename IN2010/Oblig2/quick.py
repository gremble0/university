import math
import sys
import statistics
# [1,2,6,2,5,1,1,421,5,25690128691209,68,12368,128,039601238,609128,609128,6098,1230968,1203958,123093841290,1236]
# 
def quickSort(arr, low, high, comps, swaps):
    if low >= high: 
        return [arr, comps, swaps]
    part = partition(arr, low, high, comps, swaps)
    quickSort(arr, low, part[0] - 1, part[1], part[2])
    quickSort(arr, part[0] + 1, high, part[1], part[2])
    return [arr, part[1], part[2]]

def partition(arr, low, high, comps, swaps):
    pivot = choosePivot(arr, low, high)
    arr[pivot], arr[high] = arr[high], arr[pivot]
    swaps += 1
    pivot = arr[high]
    left = low
    right = high - 1

    while left <= right:
        comps += 1
        while left <= right and arr[left] <= pivot:
            left += 1
            comps += 2
        while right >= left and arr[right] >= pivot:
            right -= 1
            comps += 2
        if left < right:
            arr[left], arr[right] = arr[right], arr[left]
            swaps += 1
            comps += 1
    
    arr[left], arr[high] = arr[high], arr[left]
    swaps += 1
    return [left, comps, swaps]

def choosePivot(arr, low, high):
    median = statistics.median([arr[low], arr[math.floor(len(arr) / 2)], arr[high]])
    if median == arr[low]:
        return low
    elif median == arr[math.floor(len(arr) / 2)]:
        return math.floor(len(arr) / 2)
    else:
        return high


def lesInput():
    arr = []
    
    input_fil = open(sys.argv[1], "r") # krever at filnavnet skrives i terminalen ved kjoering av program
    for linje in input_fil:
        arr.append(int(linje))
    input_fil.close()
    
    output_fil = open(sys.argv[1].split("\\")[-1] + "_quick.out", "w")
    for element in quickSort(arr, 0, math.floor(len(arr) - 1), 0, 0)[0]:
        output_fil.write(str(element) + "\n")
    output_fil.close()

if __name__ == "__main__":
    lesInput()