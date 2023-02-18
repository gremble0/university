def sjekk_reise(reise):
    er_gyldig = True
    # forrige_reisemaal = reise[0][1]
    for teller, reiser in enumerate(reise):
        forrige_reisemaal = reise[teller][1]
        if reiser[0] != forrige_reisemaal:
            er_gyldig = False
    return er_gyldig

print(sjekk_reise([["Spania", "Frankrike"], ["Frankrike", "Norge"]]))