-- Oppgave 1
SELECT firstname, lastname, parttype FROM film
    INNER JOIN filmparticipation USING (filmid)
    INNER JOIN person USING (personid)
    WHERE title = 'Star Wars' AND parttype = 'cast'

-- Oppgave 2

SELECT count(filmid), country
    FROM filmcountry
    GROUP BY country
    ORDER BY count DESC

-- Oppgave 3

SELECT country, avg(time::int)
        FROM runningtime
        WHERE time ~ '^\d+$'
        GROUP BY country
	HAVING count(country) >= 200

-- Oppgave 4
-- Maa grupperes etter filmid for aa ikke telle filmer med samme tittel som samme film

SELECT count(genre) AS ant_sjangre, title
    FROM filmgenre
    INNER JOIN film USING (filmid)
    GROUP BY filmid, title
    ORDER BY ant_sjangre DESC
    LIMIT 10

-- Oppgave 5

WITH filmer_i_land AS (
	SELECT count(filmid) AS ant_filmer, country
		FROM filmcountry
		GROUP BY country
),
rank_i_land AS (
	SELECT avg(rank) AS gjennomsnitt_rank, country
		FROM filmrating
		LEFT JOIN filmcountry USING (filmid)
		GROUP BY country
),
sjanger_i_land AS (
	SELECT max(genre) AS vanligst_sjanger, country
		FROM filmgenre
		LEFT JOIN filmcountry USING (filmid)
		GROUP BY country
)

SELECT * FROM sjanger_i_land
	JOIN rank_i_land USING (country)
	JOIN filmer_i_land USING (country)
	ORDER BY ant_filmer DESC

-- Oppgave 6
-- Virker ikke. Returnerer alle norske skuespillere som har spilt i mer enn 40 filmer.

SELECT personid, count(personid), lastname, firstname FROM film
	JOIN filmcountry USING (filmid)
	JOIN filmparticipation USING (filmid)
	JOIN person USING (personid)
	WHERE country = 'Norway'
	GROUP BY personid, lastname, firstname
	HAVING count(personid) > 40;

-- Oppgave 7

SELECT DISTINCT title, prodyear FROM film
	INNER JOIN filmgenre USING (filmid)
	INNER JOIN filmcountry USING (filmid)
	WHERE (title LIKE '%Dark%' OR title LIKE '%Night%')
		AND (genre = 'Horror' OR country = 'Romania')

-- Oppgave 8

SELECT count(partid), title FROM film
	LEFT OUTER JOIN filmparticipation USING (filmid)
	WHERE prodyear >= 2010
	GROUP BY title
	HAVING count(partid) <= 2

-- Oppgave 9
-- Virker ikke. Enten returnerer alle filmer som er enten Horror eller Sci-fi uten duplikater (?).
-- Proevde saa aa trekke dette fra alle filmer, men virket ikke

WITH enten AS (SELECT filmid, title, count(title), genre FROM film
	JOIN filmgenre USING (filmid)
	WHERE genre = 'Horror' or genre = 'Sci-Fi'
	GROUP BY filmid, title, genre
)

SELECT 
	(SELECT filmid, title, count(title), genre FROM film 
		JOIN filmgenre USING (filmid))
		- (SELECT * FROM enten)

-- Oppgave 10
-- Virker som det skal i mine oyne men ikke samme antall rader i output som i oppgavetekst

WITH interessante_filmer AS (
	SELECT * FROM film
		INNER JOIN filmrating USING (filmid)
		WHERE votes > 1000 AND rank > 8
),
hoyest_rank_filmer AS (
	SELECT title, filmid FROM interessante_filmer
		ORDER BY rank, votes
		LIMIT 10
),
harrison_ford_filmer AS (
	SELECT title, filmid FROM interessante_filmer
		INNER JOIN filmparticipation USING (filmid)
		INNER JOIN person USING (personid)
		WHERE firstname = 'Harrison' AND lastname = 'Ford'
),
romcom_filmer AS (
	SELECT DISTINCT title, filmid FROM interessante_filmer
		INNER JOIN filmgenre USING (filmid)
		WHERE genre = 'Romance' OR genre = 'Comedy'
)


SELECT count(language) AS antall_spraak, title FROM (
	(SELECT * FROM hoyest_rank_filmer
	UNION
	SELECT * FROM harrison_ford_filmer
	UNION
	SELECT * FROM romcom_filmer) _
	LEFT OUTER JOIN filmlanguage USING(filmid)
) 
GROUP BY title
ORDER BY antall_spraak DESC