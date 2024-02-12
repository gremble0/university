public class MatrixMultiplyInIntervalBTransposed extends MatrixMultiplyInInterval {
    public MatrixMultiplyInIntervalBTransposed(int startRow, int endRow, Matrix a, Matrix b, double[][] dest) {
        super(startRow, endRow, a, b, dest);
        b = b.transpose();
    }

    public void run() {
        for (int i = startRow; i < endRow; i++) {
            for (int j = 0; j < a.matrix[i].length; j++) {
                for (int k = 0; k < b.matrix.length; k++) {
                    dest[i][j] += a.matrix[i][k] * b.matrix[j][k];
                }
            }
        }
    }
}
