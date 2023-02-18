import java.io.File;
import java.io.FileNotFoundException;
import java.lang.reflect.Array;
import java.util.Scanner;
import java.util.regex.*;

public class Labyrint {
    private Rute[][] ruter;

    public Labyrint(String filnavn) {
        // hvis filnavnet ikke bestaar av 1 eller flere bokstaver + ".in", avslutt programmet
        if (!Pattern.compile("\\w+\\.in").matcher(filnavn).find()) {
            System.out.println("Ugyldig fil format");
            System.exit(0);
        }

        try {
            Scanner leser = new Scanner(new File(filnavn));
            String linje = leser.nextLine();
            int rader = Integer.parseInt(linje.split(" ")[0]);
            int kolonner = Integer.parseInt(linje.split(" ")[1]);
            ruter = new Rute[rader][kolonner];
            int radNr = 0;

            while (leser.hasNextLine()) {
                linje = leser.nextLine();
                for (int kolNr = 0; kolNr < linje.length(); kolNr++) {
                    if (linje.charAt(kolNr) == '#') {
                        ruter[radNr][kolNr] = new SortRute(radNr, kolNr, this);
                    } else { // antar gyldig filformat
                        // hvis linje.charAt(kolNr) ikke er lik # og en av foelgende tester er sanne vet vi at vi er ved en aapning
                        if (radNr == 0 || radNr == rader - 1 || kolNr == 0 || kolNr == kolonner - 1) {
                            ruter[radNr][kolNr] = new Aapning(radNr, kolNr, this);
                        } else {
                            ruter[radNr][kolNr] = new HvitRute(radNr, kolNr, this);
                        }
                    }
                }
                radNr++;
            }

            for (int i = 0; i < rader; i++) {
                for (int j = 0; j < kolonner; j++) {
                    try {
                        ruter[i][j].settNabo("nord", ruter[i-1][j]);
                    } catch (ArrayIndexOutOfBoundsException e) {} // ikke sett nabo hvis ruten er out of bounds (hvis vi er ved en kant)
                    try {
                        ruter[i][j].settNabo("oest", ruter[i][j+1]);
                    } catch (ArrayIndexOutOfBoundsException e) {}
                    try {
                        ruter[i][j].settNabo("soer", ruter[i+1][j]);
                    } catch (ArrayIndexOutOfBoundsException e) {}
                    try {
                        ruter[i][j].settNabo("vest", ruter[i][j-1]);
                    } catch (ArrayIndexOutOfBoundsException e) {}
                }
            }
            System.out.println("Slik ser labyrinten ut:\n" + this);
            leser.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    @Override
    public String toString() {
        String resultatString = "";
        for (int i = 0; i < ruter.length; i++) {
            for (int j = 0; j < ruter[i].length; j++) {
                resultatString += ruter[i][j];
            }
            resultatString += "\n";
        }
        return resultatString;
    }

    public void finnUtveiFra(int rad, int kol) {
        if (rad > ruter.length - 1 || kol > ruter[0].length - 1) {
            System.out.println("Gitt rad eller kolonne finnes ikke i labyrinten");
            return;
        }
        System.out.println(ruter[0].length);
        ruter[rad][kol].finn(ruter[rad][kol]);
    }
}