;;; Oppgave 1
;; a: `(* (+ 4 2) 5)' evaluerer til 30 siden uttrykket er skrevet på riktig form. Pluss 4 med 2 og så gang det med 5.

;; b: `(* (+ 4 2) (5))' fører til en error siden uttrykket prøver å kalle på prosedyren (5). Scheme lar oss heller ikke definere prosedyrer med tall som navn så denne vil aldri virke.

;; c: `(* (4 + 2) 5)' fører til en error siden uttrykket prøver å kalle på prosedyren (4) med argumentene `+' og `2'. Dette vil feile av samme årsak som uttrykk b.

;; d: `define bar (/ 44 2))
;;    bar' evaluerer til 22 siden vi i første linje definerer variabelen `bar' som evalueres til 22, men uten å printe det tilbake. Når vi spør etter variabelen `bar' på neste linje printes dette tilbake til oss.

;; e: (- bar 11) evaluerer til 11 siden den kaller på prosedyren `-' med argumentene `bar' (bundet til 22 etter uttryk d) og `11'. Så lenge bar er en bundet variabel av typen number (int, float, osv) er dette et gyldig uttryk.

;; f: `(/ (* bar 3 4 1) bar)' evaluerer til 12 siden den først multipliserer `bar * 3 * 4 * 1 = 22 * 3 * 4 *1 = 264' før dette deles på `bar' (22). Gyldig så lenge `bar' er bundet.

;;; Oppgave 2
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

;; Vi ser også i disse eksemplene at or, and og if er special forms siden de ikke bryter ned alle symbolene til primitive verdier før de ser at de må, de tar heller noen snarveier for å optimalisere prosedyrene ettersom man ofte ikke trenger å evaluere alle mulige utfall for prosedyren.

;; b:
(define (sign-if num)
  (if (zero? num)
      0
      (if (positive? num)
	  1
	  -1)))

(define (sign-cond num)
  (cond ((positive? num) 1)
	((negative? num) -1)
	(else 0)))

;; c:
(define (sign-or-and num)
  (or (and (positive? num) 1)
      (and (negative? num) -1)
      0))

;;; Oppgave 3
;; a:
(define (add1 num)
  (+ 1 num))

(define (sub1 num)
  (- num 1))

;; b:
(define (plus-iter num1 num2)
  (if (zero? num2)
      num1
      (plus-iter (add1 num1) (sub1 num2))))

;; c:
;; Min løsning i oppgave b gir opphav til en iterativ prosess siden vi kan se at den er halerekursiv.
;; Dvs. at siden det siste som skjer i prosedyren (det som er i haleposisjon) er det rekursive prosedyrekallet,
;; vil det ikke bygge seg opp en stabel av ventende prosedyrekall og minnet vil vokse lineært når num1 og num2 øker.

;; Her er en løsning som fører til en rekursiv prosess.
(define (plus-rec num1 num2)
  (if (zero? num2)
      num1
      (add1 (plus-rec num1 (sub1 num2)))))

;; d:
;; Siden (power-iter) arver mye av variablene den bruker fra (power-close-to) gir det mening å definere denne prosedyren 
;; lokalt under for (power-close-to). Da trenger vi ikke ta b og n som parametre til (power-iter) prosedyren siden de
;; allerede er synlige og ikke modifiseres for de rekursive prosedyrekallene.
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
	e
	(power-iter (+ 1 e))))
  (power-iter 1))

;; e:
;; Siden (fib-iter) modifiserer argumentene a og b i sine rekursive prosedyrekall (i kallet på (+ a b) er det ikke mulig
;; å fjerne noen av parametrene til den indre prosedyren.
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
