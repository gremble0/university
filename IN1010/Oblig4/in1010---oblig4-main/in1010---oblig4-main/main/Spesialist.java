public class Spesialist extends Lege implements Godkjenningsfritak{
    protected String kontrollID;

    Spesialist(String legenavn, String kontrollID){
        super(legenavn);
        this.kontrollID = kontrollID;
    }

    @Override
    public String hentKontrollID(){
        return kontrollID;
    }

    @Override
    public String toString(){
        return String.format("\n%s er en spesialist lege.\nKontroll ID: %s", super.toString(), kontrollID);
    }
}
