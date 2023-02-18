teller = 1
sum = 0
while teller <= 10:
    sum += teller
    teller += 1

# print(sum)

# WHILE

tall = [12, 16, 15, 16]
innenfor = True
teller = 0 
while teller < len(tall):
    if tall[teller] <= 10 or tall[teller] >= 20:
        innenfor = False
        break
    teller += 1

# FOR LØKKE

tall = [12, 16, 15, 16]
innenfor = True
for x in tall:
    if x <= 10 or x >= 20:
        innenfor = False
        break
    teller += 1

print(innenfor)