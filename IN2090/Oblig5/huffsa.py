import psycopg2

user = 'hermagst'
pwd = ''

connection = f"dbname='{user}'user='{user}_priv'port='5432'host='dbpg-ifi-kurs03.uio.no'password='{pwd}'"

def huffsa():
    conn = psycopg2.connect(connection)
    
    ch = 0
    while (ch != 3):
        print("--[ HUFFSA ]--")
        print("Vennligst velg et alternativ:\n 1. Søk etter planet\n 2. Legg inn forsøksresultat\n 3. Avslutt")
        ch = int(input("Valg: "))

        if (ch == 1):
            planet_sok(conn)
        elif (ch == 2):
            legg_inn_resultat(conn)
    
def planet_sok(conn):
    molekyl1 = input("Molekyl 1: ")
    molekyl2 = input("Molekyl 2: ")
    cur = conn.cursor()

    if molekyl2 == "":
        cur.execute("SELECT p.navn, p.masse, s.masse, s.avstand, p.liv FROM stjerne s \
	                JOIN planet p ON (s.navn = p.stjerne) \
	                JOIN materie AS m ON (m.planet = p.navn) \
	                WHERE molekyl = %s", (molekyl1,))
    else:
        cur.execute("SELECT p.navn, p.masse, s.masse, s.avstand, p.liv \
                    FROM stjerne AS s JOIN planet AS p ON (s.navn = p.stjerne) \
	                JOIN materie AS m ON (m.planet = p.navn) \
	                WHERE molekyl in (%s, %s)  \
	                GROUP BY p.navn, s.masse, s.avstand \
	                HAVING count(*) > 1;", (molekyl1, molekyl2))
    
    row = cur.fetchone()
    while row is not None:
        print(f"--Planet--\n" \
              f"Navn: {row[0]}\n" \
              f"Planet-masse: {row[1]}\n" \
              f"Stjerne-masse: {row[2]}\n" \
              f"Stjerne-distanse: {row[3]}\n" \
              f"Bekreftet liv: {'Ja' if row[4] == 't' else 'nei'}\n")
        row = cur.fetchone()
        
    conn.commit()


def legg_inn_resultat(conn):
    inpNavn = input("Planet: ")
    inpSkummel = "t" if input("Skummel: ") == "j" else "f"
    inpIntelligent = "t" if input("Intelligent: ") == "j" else "f"
    inpBeskrivelse = input("Beskrivelse: ")

    cur = conn.cursor()
    cur.execute("UPDATE planet \
                SET skummel = %s, \
                intelligent = %s, \
                beskrivelse = %s \
                WHERE navn = %s", (inpSkummel, inpIntelligent, inpBeskrivelse, inpNavn))
    conn.commit()
    
    print("Resultat lagt inn\n")

if __name__ == "__main__":
    huffsa()