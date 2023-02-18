import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.Stack;
import java.util.regex.Pattern;

public class Oblig6 {
    public static void main(String[] args) {
        Labyrint lab = new Labyrint(args[0]); // filer maa vaere i samme mappe eller oppgitt med path i args
        System.out.println("Skriv inn koordinater <rad> <kolonne> ('-1' for aa avslutte)");
        Scanner input = new Scanner(System.in);
        String linje = input.nextLine();

        while (!linje.equals("-1")) {
            // hvis input ikke er paa formen: 1 eller fler tall, mellomrom, 1 eller fler tall; ignorer input
            if (!Pattern.compile("\\d+ \\d+").matcher(linje).find()) {
                System.out.println("Ugylding input");
                linje = input.nextLine();
                continue;
            }

            lab.finnUtveiFra(
                Integer.parseInt(linje.split(" ")[0]),
                Integer.parseInt(linje.split(" ")[1])
            );

            System.out.println("\nSkriv inn koordinater <rad> <kolonne> ('-1' for aa avslutte)");
            linje = input.nextLine();
        }
        input.close();
    }
}
