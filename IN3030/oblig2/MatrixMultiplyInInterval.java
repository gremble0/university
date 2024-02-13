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

    /**
     * Runnable for multiplying two matrices where neither have been transposed
     *
     */
    public static class NotTransposed extends MatrixMultiplyInInterval {
        public NotTransposed(int startRow, int endRow, Matrix a, Matrix b, double[][] dest) {
            super(startRow, endRow, a, b, dest);
        }

        public void run() {
            for (int i = startRow; i < endRow; i++) {
                for (int j = 0; j < a.matrix[i].length; j++) {
                    for (int k = 0; k < b.matrix.length; k++) {
                        dest[i][j] += a.matrix[i][k] * b.matrix[k][j];
                    }
                }
            }
        }
    }

    /**
     * Runnable for multiplying two matrices where a has already been transposed
     *
     */
    public static class ATransposed extends MatrixMultiplyInInterval {
        public ATransposed(int startRow, int endRow, Matrix a, Matrix b, double[][] dest) {
            super(startRow, endRow, a, b, dest);
        }

        public void run() {
            for (int i = startRow; i < endRow; i++) {
                for (int j = 0; j < a.matrix[i].length; j++) {
                    for (int k = 0; k < b.matrix.length; k++) {
                        dest[i][j] += a.matrix[k][i] * b.matrix[k][j];
                    }
                }
            }
        }
    }

    /**
     * Runnable for multiplying two matrices where b has already been transposed
     *
     */
    public static class BTransposed extends MatrixMultiplyInInterval {
        public BTransposed(int startRow, int endRow, Matrix a, Matrix b, double[][] dest) {
            super(startRow, endRow, a, b, dest);
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
}
