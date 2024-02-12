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
                return b.multiplyParaTransposed(pat);

            case PARA_B_TRANSPOSED:
                Matrix pbt = b.transpose();
                return multiplyParaTransposed(pbt);

            default:
                throw new EnumConstantNotPresentException(mode.getClass(), mode.getClass().getName());
        }
    }

    private Matrix multiplySeq(Matrix b) {
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

    private Matrix multiplyPara(Matrix b) {
        return null;
    }

    private Matrix multiplyParaTransposed(Matrix transposed) {
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
