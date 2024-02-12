import java.util.Arrays;

public class Matrix {
    public double[][] matrix;

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
        double[][] multiplied = new double[matrix.length][matrix[0].length];

        switch (mode) {
            case SEQ_NOT_TRANSPOSED:
                new MatrixMultiplyInIntervalNotTransposed(0, matrix.length, this, b, multiplied).run();
                break;

            case SEQ_A_TRANSPOSED:
                new MatrixMultiplyInIntervalATransposed(0, matrix.length, this, b, multiplied).run();
                break;

            case SEQ_B_TRANSPOSED:
                new MatrixMultiplyInIntervalBTransposed(0, matrix.length, this, b, multiplied).run();
                break;

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

        return new Matrix(multiplied);
    }

    private Matrix multiplyPara(Matrix b) {
        final int cores = Runtime.getRuntime().availableProcessors();
        final int intervalSize = matrix.length / cores;
        final Thread[] threads = new Thread[cores];

        double[][] multiplied = new double[matrix.length][matrix[0].length];

        int start = 0;
        for (int i = 0; i < cores - 1; i++) {
            threads[i] = new Thread(new MatrixMultiplyInIntervalNotTransposed(start, start + intervalSize, this, b, multiplied));
            threads[i].start();
            
            start += intervalSize;
        }
        threads[cores - 1] = new Thread(new MatrixMultiplyInIntervalNotTransposed(start, matrix.length, this, b, multiplied));
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
