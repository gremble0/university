# Process vs Tråd:
- Process krever context switch (her brukes CPU sykler for å overføre context til prosessen)
- Med tråder deler "de kjørbare enhetene"/trådene minneområde og annen global data som gjør at de slipper mye context switching. NB: De har fortsatt data lokalt for hver tråd.
- Endring fra ekskvering av en tråd til en annen er mye billigere enn fra en prosess til en annen.

## Forking:
- Hvis fork() returnerer noe annet enn 0 betyr det at det er en child-prosess
- wait() venter på at noen av barneprosessene er ferdige med sin kjøring
- execve() redigerer kjøringen av prosessen til et annet program. Kode etter en execve() vil ikke utføres ettersom execve() ikke returnerer tilbake til der funksjonen ble kalt fra.
- Rekkefølgen et program utføres i ved bruk av fork() kan være litt tilfeldig (avhenger av scheduler).

# CPU scheduling:
- Vi ønsker å schedule en prosess, tråd, jobb, e.l. 
- Utfordring - mange ønsker tilgang til prosessen, dette håndterer schedulereren hvor det finnes mange forskjellige algoritmer. Prosesser har ofte også ulike prioriteringer.
- Dispatcher: litt synonymt med schedulerer. Scheduler velger hvem som får tilgang til prosessor, dispatcher utfører handlingen av å tildele dette.
- Scheduler må optimalisere CPU utilisering. 
- Prosesser er ofte flaskehalset av enten CPU(CPU bound) eller IO(IO bound). CPU bound betyr at prosessen gjør mye kalkulasjoner på CPU med lite ventetid for IO, mens IO bound er motsatt. 
- Hvordan skal vi sortere et sett av prosesser, der noen er CPU bundet og noen er IO bundet?
	- Kjøre en type først og en annen etterpå er ueffektivt siden da er enten CPU eller disk ubrukt i lange perioder. (CPU ubrukt ved eksekvering av IO bundede prosesser, og disk ubrukt for eksekvering av CPU bundede prosesser)
	- En blandet rekkefølge av CPU bundede og IO bundede prosesser er foretrukket.

## Algoritmer for scheduling:
- FIFO (first in first out):
	Kjør prosesser til de er ferdige i en kø
	+ simpel
	+ rettferdig?
	+ veldig litt overhead
	- lang ventetid og avslutningstid
- SJF (shortest job first):
	Sorter prosesser etter kostnad og kjør dem i kø
	+ simpel
	+ bedre gjennomsnittlige tider enn FIFO
	- vanskelig å bestemme kostnad på forhånd
	- muligens lange avslutningstider og utsultning hvis nye korte jobber ankommer.
- RR (round robin):
	Dette er en variant av FIFO der vi deler opp prosesser i mindre tidsbiter (f.eks 10ms) sånn at vi unngår mange av problemene med FIFO.
	+ alle programmer får lov til å kjøre (alle prosesser får tildelt tid med en gang)
	- mange context switches kan gjøre at dette tar lengre tid totalt
	- ingen prosesser kan bli "heldige" og bli ferdige fort

## Hvordan bestemme lengden på tidsbiter for RR?
- Lange eller korte? 
	- Dette avhenger av hvor mye tid prosesser trenger på CPU sammenlignet med disk. 
		- F.eks ved tidsbit på 100ms med prosesser A,B og C der A og B kjører for evig og C løkker for evig med en alternering mellom 1ms på CPU og 10ms på disk, vil C sultes for tid på CPU (CPU bound). Dette gir oss dårlig disk utilization (5%)
		- Ved tidsbit på 1ms med de samme prosessene får vi mye høyere disk utilization (91%).
	- CPU bundede prosesser liker lange tidsbiter, IO bundede liker korte tidsbiter (siden de også avhenger av IO enheter og ikke bare CPU).

## Egenskaper en scheduler må ta hensyn til:
- Behandle like oppgaver på like måter
- Ingen prosesser burde vente for evig
- Kort responstid
- Maksimere throughput - få gjort ferdig så mange oppgaver så fort som mulig
- Maksimere bruk av PCens ressurser (mellom 40-90% er vanlig) - balanse, alle komponenter skal benyttes
- Minimere overhead - tid brukt på andre ting enn direkte prosessering
- Forutsigbar tilgang til CPU

- Kernel - ressurshåndtering, prosessor utilization, throughput, rettferdighet
- Bruker - ønsker blant annet kort responstid og konsekventhet (PCen skal ikke kræsje, programmer skal ikke f.eks ta mye lengre tid å åpne en gang)
- Brukeren av systemet: Servere vil som regel ha lengre tidsbiter, brukersystemer vil ha kortere.

## Scheduleringsklassifiseringer:
### Dynamisk schedulering:
- Tilpasser seg ved kjøretid
- Fleksibel
- Flere beregninger - mer overhead

### Statisk schedulering:
- Lager scheduleringsregler før kjøretid
- Genererer dispatching tabell for kjøretids-dispatcheren ved kompileringstid
- Trenger kunnskap om oppgaven før kompilering

## Kan man avbryte en oppgave?
### Preemptive schedulering:
- Kjøring av oppgaver kan avbrytes ("preemptes") av prosesser med høyere prioritet
- Avbrutte prosesser fortsetter senere med samme tilstand
- Mer overhead
- Real-time prioriteres fremfor best effort.
	- Real-time prosesser vil alltid kjøres hvis de finnes
- Når skal vi tillate "preemption"?
	- Preemption points - scheduler vil sjekke om det finnes noen høyt prioriterte oppgaver den må slippe til - forutsigbart overhead - liten forsinkelse for håndtering av real-time prosesser
	- Immediate preemption - nødvendig for harde real-time systemer (brukersystemer?) - mer overhead siden man må ta vare på tilstanden til den avbrutte prosessen - ingen forsinkelse for håndtering av real-time prosesser

### Non-preemptive:
- Oppgaver med høyere prioritet må vente på at oppgaven som prosesseres nå fullfører - kan også brukes uten prioritet
- Mindre overhead i forhold til context switcher
- Med prioritet har håndtering av real-time prosesser en liten forsinkelse.

I dag er dynamisk og preemptive scheduleringsalgoritmer vanligst, men alle brukes. Det fleste systemer har et slags form for prioritering.

## Prioritetsscheduling:
- Scheduler lager flere køer for hver prioritet. Alle prosesser fra kø med høyest prioritet vil utføres før scheduler går over til neste kø
	- Fordel - rettferdighet (så lenge tildeling av prioriteter gjøres riktig)
	- Ulempe - utsulting, noen prosesser må kanskje vente veldig lange - løsning: dynamiske prioriteter som endrer prioriteten på prosesser når de har ventet for lenge, men dette gir mer overhead.

### Microsoft windows 2000: Forgrunnsaktiviteter gis lengre tidsbiter siden de trenger rask responstid
	- 32 prioritetsnivåer med RR i hvert nivå
	- Input og throughput orientert
	- De 16 øverste nivåene: real-time prosesser med statiske prioriteter som kan kjøre for evig
	- De 15 neste nivåene: prioritet kan økes eller synkes med 2 nivåer. CPU bundede prosesser får redusert prioritet. IO bundede prosesser får økt prioritet. Dette er for å øke interaktivitet med systemet.
	- Det siste nivået er for veldig lavt prioriterte OS prosesser
### Windows 8/10: For det meste det samme, med 2 nye grupperinger som skiller mellom prosessprioritet og trådprioritet.
	- Hver prosessklasse har 7 ekstra trådprioriteter.
	- For å finne den endelige trådprioriteten for en prosess tar man hensyn til begge disse
	- Dynamiske prioriteter for nivå 0-15
	- Bruker kan også lage egen schedulering for hver applikasjon (user mode scheduling, UMS)
	- Kan også gi garanti for multimedia prosesser (MMCSS)

### Linux:
- 3 prioritetsklasser:
	- SCHED_FIFO - prosesser kan kjøre for evig uten tidsbiter
	- SCHED_RR - mykere versjon av SCHED_FIFO, tidsbiter på 10ms(quantums)
	- SCHED_OTHER - for vanlige brukerprosesser - 40 (nice-verdier) prioriteter med tidsbiter på 10ms(quantums)
- Tråder med høyest "goodness" kjører først.
	- For real-time prosesser under SCHED_FIFO og SCHED_RR: goodness = 1000 + prioritetsnivå
	- For timesharing (bruker prosesser) under SCHED_OTHER: goodness = tid igjen for prosessen + prioritet
- Quantums resettes når ingen klare prosesser har quantums igjen (end of epoch)
- Denne løsningen var ikke helt bra - gikk over til CFS
### Nyere linux - Completely Fair Scheduler (CFS):
- Bare en kø: Alle får sin rettferdige del av en tidsbit - F.eks. hvis du har en tidsbit på 10ms og to prosesser har samme prioritet deler de den tidsbiten opp i 5ms * 2
- Hver prosess sorteres etter virtuel kjøretid, de med lavest får høyest prioritet
- Bruker kun et rødsvart tre for køen av prosesser
	- Unngår å bruke mange separate køer schedulerer må søke gjennom.
	- For å finne neste prosess å kjøre går man bare lengst ned til venstre i treet
- Hvis 2 brukere køer opp prosesser vil hver bruker få 50% hver og så dele opp hver brukers tildelt prioritet på deres prosesser.

## Når skal scheduler brukes?:
- Når en ny prosess lages
- Når en prosess er ferdig
- Når en prosess blokker
- Når en prosess avbrytes
- I preemptive systemer: ved preemption points.
