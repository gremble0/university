import java.util.Arrays;

public class Matrix {
    public double[][] matrix;
    private class MatrixMultiplyInInterval implements Runnable {
        private final int startRow;
        private final int endRow;
        private double[][] b;
        private double[][] multiplied;

        public MatrixMultiplyInInterval(int startRow, int endRow, double[][] b, double[][] multiplied) {
            this.startRow = startRow;
            this.endRow = endRow;
            this.b = b;
            this.multiplied = multiplied;
        }

        public void run() {
            for (int i = startRow; i < endRow; i++) {
                for (int j = 0; j < matrix[i].length; j++) {
                    for (int k = 0; k < b.length; k++) {
                        multiplied[i][j] += matrix[i][k] * b[k][j];
                    }
                }
            }
        }
    }

    public Matrix(double[][] matrix) {
        this.matrix = matrix;
    }

    public Matrix transpose() {
        // TODO: Check if its faster to not copy and loop over more of the matrix instead
        double[][] transposed = deepCopy();

        for (int row = 0; row < matrix.length; row++) {
            for (int col = row; col < matrix[row].length; col++) {
                transposed[row][col] = matrix[col][row];
                transposed[col][row] = matrix[row][col];
            }
        }

        return new Matrix(transposed);
    }

    public Matrix multiply(Matrix b, Oblig2Precode.Mode mode) {
        switch (mode) {
            case SEQ_NOT_TRANSPOSED:
                return multiplySeq(b);

            case SEQ_A_TRANSPOSED:
                Matrix sat = transpose();
                return sat.multiplySeqATransposed(b);

            case SEQ_B_TRANSPOSED:
                Matrix sbt = b.transpose();
                return multiplySeqBTransposed(sbt);

            case PARA_NOT_TRANSPOSED:
                return multiplyPara(b);

            case PARA_A_TRANSPOSED:
                Matrix pat = transpose();
                return pat.multiplyParaATransposed(b);

            case PARA_B_TRANSPOSED:
                Matrix pbt = b.transpose();
                return multiplyParaBTransposed(pbt);

            default:
                throw new EnumConstantNotPresentException(mode.getClass(), mode.getClass().getName());
        }
    }

    private Matrix multiplySeq(Matrix b) {
        // TODO: use private class ?
        double[][] multiplied = new double[matrix.length][matrix[0].length];

        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[i].length; j++) {
                for (int k = 0; k < b.matrix.length; k++) {
                    multiplied[i][j] += matrix[i][k] * b.matrix[k][j];
                }
            }
        }

        return new Matrix(multiplied);
    }

    private Matrix multiplySeqATransposed(Matrix b) {
        double[][] multiplied = new double[matrix.length][matrix[0].length];

        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[i].length; j++) {
                for (int k = 0; k < b.matrix.length; k++) {
                    multiplied[i][j] += matrix[k][i] * b.matrix[k][j];
                }
            }
        }

        return new Matrix(multiplied);
    }

    private Matrix multiplySeqBTransposed(Matrix b) {
        double[][] multiplied = new double[matrix.length][matrix[0].length];

        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[i].length; j++) {
                for (int k = 0; k < b.matrix.length; k++) {
                    multiplied[i][j] += matrix[i][k] * b.matrix[j][k];
                }
            }
        }

        return new Matrix(multiplied);
    }

    private Matrix multiplyPara(Matrix b) {
        final int cores = Runtime.getRuntime().availableProcessors();
        final int intervalSize = matrix.length / cores;
        final Thread[] threads = new Thread[cores];

        double[][] multiplied = new double[matrix.length][matrix[0].length];

        int start = 0;
        for (int i = 0; i < cores - 1; i++) {
            threads[i] = new Thread(new MatrixMultiplyInInterval(start, start + intervalSize, b.matrix, multiplied));
            threads[i].start();
            
            start += intervalSize;
        }
        threads[cores - 1] = new Thread(new MatrixMultiplyInInterval(start, matrix.length, b.matrix, multiplied));
        threads[cores - 1].start();

        try {
            for (int i = 0; i < threads.length; i++) {
                threads[i].join();
            }
        } catch(Exception e) {
            System.out.println(e);
            System.exit(1);
        }

        return new Matrix(multiplied);
    }

    private Matrix multiplyParaATransposed(Matrix transposed) {
        return null;
    }

    private Matrix multiplyParaBTransposed(Matrix transposed) {
        return null;
    }

    private double[][] deepCopy() {
        double[][] copied = new double[matrix.length][matrix[0].length];

        for (int i = 0; i < matrix.length; i++) {
            copied[i] = Arrays.copyOf(matrix[i], matrix[i].length);
        }

        return copied;
    }

    public void print() {
        for (int i = 0; i < matrix.length; i++) {
            System.out.println(Arrays.toString(matrix[i]));
        }
    }
}
