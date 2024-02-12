public abstract class MatrixMultiplyInInterval implements Runnable {
    protected final int startRow;
    protected final int endRow;
    protected Matrix a;
    protected Matrix b;
    protected double[][] dest;

    protected MatrixMultiplyInInterval(int startRow, int endRow, Matrix a, Matrix b, double[][] dest) {
        this.startRow = startRow;
        this.endRow = endRow;
        this.a = a;
        this.b = b;
        this.dest = dest;
    }
}
