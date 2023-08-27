					; Oppgave 1
;; a: `(* (+ 4 2) 5)' evaluerer til 30 siden uttrykket er skrevet på riktig form. Pluss 4 med 2 og så gang det med 5.

;; b: `(* (+ 4 2) (5))' fører til en error siden uttrykket prøver å kalle på prosedyren (5). Scheme lar oss heller ikke definere prosedyrer med tall som navn så denne vil aldri virke.

;; c: `(* (4 + 2) 5)' fører til en error siden uttrykket prøver å kalle på prosedyren (4) med argumentene `+' og `2'. Dette vil feile av samme årsak som uttrykk b.

;; d: `define bar (/ 44 2))
;;    bar' evaluerer til 22 siden vi i første linje definerer variabelen `bar' som evalueres til 22, men uten å printe det tilbake. Når vi spør etter variabelen `bar' på neste linje printes dette tilbake til oss.

;; e: (- bar 11) evaluerer til 11 siden den kaller på prosedyren `-' med argumentene `bar' (bundet til 22 etter uttryk d) og `11'. Så lenge bar er en bundet variabel av typen number (int, float, osv) er dette et gyldig uttryk.

;; f: `(/ (* bar 3 4 1) bar)' evaluerer til 12 siden den først multipliserer `bar * 3 * 4 * 1 = 22 * 3 * 4 *1 = 264' før dette deles på `bar' (22). Gyldig så lenge `bar' er bundet.

					; Oppgave 2
;; a:
;; `(or (= 1 2)
;;      "paff!"
;;      "piff!"
;;      (zero? (1 - 1)))' evaluerer til "paff!" siden det er det første uttrykket gitt til (or) som evalueres til noe annet enn #f. (or) vil da avslutte tidlig siden den allerede vet returverdien sin, og hverken "piff!", (zero?) eller syntaksfeilen (1 - 1) evalueres.
;; `(and (= 1 2)
;;       "paff!"
;;       "piff!"
;;       (zero? (1 - 1)))' evaluerer til #f siden det første uttrykket (= 1 2) evalueres til #f. (and) vil da avslutte tidlig siden den da allerede vet returverdien sin, og hverken strengene "paff!" og "piff!", (zero?) eller syntaksfeilen (1 - 1) evalueres.
;; `(if (positive? 42)
;;      "poff!"
;;      (i-am-undefined))' evaluerer til "poff!" siden resultaten til (positive? 42) er #t. (if) vet derfor at den skal returnere sitt andre argument "poff!" fremfor det tredje argumentet (i-am-undefined) som derfor aldri evalueres.
;; 

;; b:
