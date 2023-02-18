class TestBaathus {
    public static void main(String[] args) {
        Baathus mittBaathus = new Baathus(3);
        Baat baat1 = new Baat("Opel");
        Baat baat2 = new Baat("Volkswagen");
        Baat baat3 = new Baat("BMW");

        mittBaathus.settInn(baat1);
        mittBaathus.settInn(baat2);
        mittBaathus.settInn(baat3);
    }
}