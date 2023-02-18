import sys
import math

class Binaerheap:
    def __init__(self):
        self.heap = []
        self.comps = 0
        self.swaps = 0
    
    def parentOf(self, i):
        return math.floor((i-1) / 2)
    
    def leftOf(self, i):
        return 2 * i + 1
    
    def rightOf(self, i):
        return 2 * i + 2

    def insert(self, i):
        index = len(self.heap)
        self.heap.append(i)
        while self.heap[self.parentOf(index)] > i and index > 0:
            self.comps += 2
            self.heap[index] = self.heap[self.parentOf(index)]
            self.heap[self.parentOf(index)] = i
            index = self.parentOf(index) 

    # tatt fra heapq, endret til aa telle compares og swaps + objektorientert + noen endringer
    def heappop(self):
        lastelt = self.heap.pop()
        self.comps += 1
        if self.heap:
            returnitem = self.heap[0]
            self.heap[0] = lastelt
            self.swaps += 1
            self.siftup(0)
            return returnitem
        return lastelt

    def siftdown(self, startpos, pos):
        newitem = self.heap[pos]
        while pos > startpos:
            self.comps += 1
            parentpos = self.parentOf(pos)
            parent = self.heap[parentpos]
            if newitem < parent:
                self.heap[pos] = parent
                self.swaps += 1
                pos = parentpos
                continue
            break
        self.heap[pos] = newitem

    def siftup(self, pos):
        endpos = len(self.heap)
        startpos = pos
        newitem = self.heap[pos]
        childpos = self.leftOf(pos)
        while childpos < endpos:
            self.comps += 1
            rightpos = childpos + 1
            if rightpos < endpos and not self.heap[childpos] < self.heap[rightpos]:
                self.comps += 1
                childpos = rightpos
            self.heap[pos] = self.heap[childpos]
            self.swaps += 1
            pos = childpos
            childpos = self.leftOf(pos)
        self.heap[pos] = newitem
        self.siftdown(startpos, pos)


def heapSort(arr): # ikke in-place
    heap = Binaerheap()
    
    for element in arr:
        heap.insert(element)

    sortertArr = []
    for _ in range(0, len(arr)):
        sortertArr.append(heap.heappop())
    return [sortertArr, heap.comps, heap.swaps]

def lesInput():
    arr = []
    
    input_fil = open(sys.argv[1], "r") # krever at filnavnet skrives i terminalen ved kjoering av program
    for linje in input_fil:
        arr.append(int(linje))
    input_fil.close()
    
    output_fil = open(sys.argv[1].split("\\")[-1] + "_heap.out", "w")
    for element in heapSort(arr)[0]:
        output_fil.write(str(element) + "\n")
    output_fil.close()

if __name__ == "__main__":
    lesInput()