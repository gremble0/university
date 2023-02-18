public class HovedprogramGAMMEL {
    public static void main(String[] args) {
        Dataklynge saga = new Dataklynge();
        for (int i = 0; i < 450; i++) {
            Node tempNode = new Node(4, 512);
            saga.leggTilNodeIRack(tempNode);
        }
        for (int i = 0; i < 16; i++) {
            Node tempNode = new Node(8, 1024);
            saga.leggTilNodeIRack(tempNode);
        }

        System.out.println("Noder med minst 128 GB: " + saga.noderMedNokMinneTotalt(128));
        System.out.println("Noder med minst 512 GB: " + saga.noderMedNokMinneTotalt(512));
        System.out.println("Noder med minst 1024 GB: " + saga.noderMedNokMinneTotalt(1024));
        System.out.println("Antall prosessorer: " + saga.antProsessorerTotalt());
        System.out.println("Antall Rack: " + saga.antRacks());
    } 
}