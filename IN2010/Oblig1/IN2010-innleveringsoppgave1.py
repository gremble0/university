import math
import sys
import heapq

# class Node:
#     def __init__(self, value):
#         self.value = value
#         self.parent = None
#         self.left = None
#         self.right = None
    
#     def setParent(self, parent):
#         self.parent = parent
    
#     def setLeft(self, left):
#         self.left = left

#     def setRight(self, right):
#         self.right = right

# class Set:
#     def __init__(self):
#         self.size = 0
#         self.root = None
    
#     def insert(self, x):
#         node = Node(x)
#         if self.size() == 0:
#             self.root = node
#             self.size += 1
#             return
        
#         nodePeker = self.root
#         for i in range(1, self.size()):
#             if nodePeker.right:
#                 pass

#         self.size += 1
        
    
#     def size(self):
#         return self.size

# def balancearray(sortertArr, index):
    # if median == None:
    #     median = sortertArr[math.floor(len(sortertArr)/2)]
    #     print(median)
    #     medianHoy = math.floor(len(sortertArr[median:-1]))
    #     balancearray(sortertArr, medianHoy)
    #     medianLav = math.floor()

    # else:
    #     medianHoy = math.floor((median + len(sortertArr))/2)
    #     medianLav =
    # sortertArr = 
    # balancearray(sortertArr, median)

    # print(sortertArr)
    # print(sortertArr[median])
    # sortertArr.remove(sortertArr[median])
    # sortertArr = sortertArr[median:len(sortertArr)]

    # if index == None:
    #     print(sortertArr[math.floor(len(sortertArr)/2)])
    #     balancearray(sortertArr, math.floor(len(sortertArr)*0.75))
    #     balancearray(sortertArr, math.floor(len(sortertArr)*0.25))
    # elif index > len(sortertArr)/2 and index < len(sortertArr):
    #     print(sortertArr[index])
    #     balancearray(sortertArr, math.floor(index + len(sortertArr)/2))
    # elif index <= len(sortertArr)/2 and index >= 0:
    #     print(sortertArr[index])
    #     balancearray(sortertArr, math.floor(index + len(sortertArr)/2))

    # if index == None:
    # print(sortertArr[math.floor(len(sortertArr)/2)])
    # balancearray(sortertArr[math.floor(len(sortertArr)/2):len(sortertArr)-1])
    # balancearray(sortertArr[0:math.floor(len(sortertArr)/2)-1])
    
    # sortertArr[math.floor(len(sortertArr))]
    

# (balancearray([0,1,2,3,4,5,6,7,8,9,10], None))

# import heapq

# def heapbalancearray(sortertArr):
#     heapq.heapify(sortertArr)
#     # for element in sortertArr:
#     #     heapq.heappush(heapArr, element)
#     print(sortertArr)


# heapbalancearray([0,1,2,3,4,5,6,7,8,9,10])


#4a

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

# lesInput()

#4b

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