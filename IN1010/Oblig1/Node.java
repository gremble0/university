public class Node {
    private int prosessorer;
    private int minne;

    public Node(int pros, int min) {
        prosessorer = pros;
        minne = min;
    }

    public int antProsessorerINode() {
        return prosessorer;
    }

    public boolean nokMinne(int paakrevdMinne) {
        return minne >= paakrevdMinne;
    }
}