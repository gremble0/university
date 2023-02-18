class Hovedprogram {
    public static void main(String[] args) {
        Snake snake = new Snake();
        Thread kjoer = new Thread(new Kjoer(snake));
        kjoer.start();
    }
}