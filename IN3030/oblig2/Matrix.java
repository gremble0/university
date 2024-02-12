import java.lang.reflect.Constructor;

public class Matrix {
    public double[][] matrix;

    public Matrix(double[][] matrix) {
        this.matrix = matrix;
    }

    public Matrix transpose() {
        double[][] transposed = new double[matrix.length][matrix[0].length];

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
                multiplyParallel(b, multiplied, MatrixMultiplyInIntervalNotTransposed.class);
                break;

            case PARA_A_TRANSPOSED:
                multiplyParallel(b, multiplied, MatrixMultiplyInIntervalATransposed.class);
                break;

            case PARA_B_TRANSPOSED:
                multiplyParallel(b, multiplied, MatrixMultiplyInIntervalBTransposed.class);
                break;

            default:
                throw new EnumConstantNotPresentException(mode.getClass(), mode.getClass().getName());
        }

        return new Matrix(multiplied);
    }

    private void multiplyParallel(Matrix b, double[][] dest, Class<? extends MatrixMultiplyInInterval> multiplier) {
        final int cores = Runtime.getRuntime().availableProcessors();
        final int intervalSize = matrix.length / cores;
        final Thread[] threads = new Thread[cores];

        try {
             Constructor<? extends MatrixMultiplyInInterval> matrixConstructor = multiplier
                 .getDeclaredConstructor(int.class, int.class, Matrix.class, Matrix.class, double[][].class);

            int start = 0;
            for (int i = 0; i < cores - 1; i++) {
                threads[i] = new Thread(matrixConstructor.newInstance(start, start + intervalSize, this, b, dest));
                threads[i].start();

                start += intervalSize;
            }
            threads[cores - 1] = new Thread(matrixConstructor.newInstance(start, matrix.length, this, b, dest));
            threads[cores - 1].start();

            for (int i = 0; i < threads.length; i++) {
                threads[i].join();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
