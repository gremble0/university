# Oppgave 2:
# 2.1
brus = input("Kunne du tenkt deg en brus? (ja/nei) ")
# 2.2
if brus.lower() == "ja": # konverterer til små bokstaver i tilfelle varianse
    print("Her har du en brus!")
elif brus.lower() == "nei":
    print("Den er grei.")
else:
    print("Det forsto jeg ikke helt")