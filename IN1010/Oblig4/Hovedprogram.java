public class Hovedprogram {
    public static void main(String[] args) {
        // Vanlig vanlig = new Vanlig("Vanlig", 1000, 100);
        // Narkotisk narkotisk = new Narkotisk("Narkotisk", 500, 5, 5);
        // Vanedannende vanedannende = new Vanedannende("Vanedannende", 100, 1, 1);

        // Lege lege = new Lege("lege");
        // Spesialist spesialist = new Spesialist("spesialist", "abc123");

        // HvitResept hvitResept = new HvitResept(vanlig, lege, 1, 1);
        // MilResept milResept = new MilResept(narkotisk, lege, 2);
        // PResept pResept = new PResept(vanedannende, spesialist, 3, 2);
        // BlaaResept blaaResept = new BlaaResept(vanlig, spesialist, 4, 4);


        // System.out.println(vanlig + "\n" + narkotisk + "\n" + vanedannende + "\n");
        // System.out.println(lege +"\n" + spesialist + "\n");
        // System.out.println(hvitResept + "\n" + milResept + "\n" + pResept + "\n" + blaaResept);


        Legesystem asd = new Legesystem();
        asd.lesFraFil("asd.txt");
        asd.mainLoop();
        asd.skrivTilFil();
        //asd.qwerty();
        //asd.mainLoop();
    }
}
