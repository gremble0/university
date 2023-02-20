# 5.1
# Lag ett program som spør brukeren om hvilken måned de er født i og svar med tilsvarende årstid
# 5.2
bursdag = input ("Hvilken måned er du født i? ")
bursdag = bursdag.lower() # ikke påkrevd
if bursdag == "desember" or bursdag == "januar" or bursdag == "februar":
    print("Du har bursdag om vinteren!")
elif bursdag == "mars" or bursdag == "april" or bursdag == "mai":
    print("Du har bursdag om våren!")
elif bursdag == "juni" or bursdag == "juli" or bursdag == "august":
    print("Du har bursdag om sommeren!")
elif bursdag == "september" or bursdag == "oktober" or bursdag == "november":
    print("Du har bursdag om høsten!")
else:
    print(bursdag, "er ikke en måned på kalenderen.")