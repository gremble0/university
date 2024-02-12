import java.util.Arrays;

public class Matrix {
    private int[][] matrix;

    public Matrix(int[][] matrix) {
        this.matrix = matrix;
    }

    public void transpose() {
        for (int row = 0; row < matrix.length; row++) {
            for (int col = row; col < matrix[row].length; col++) {
                int at_ij = matrix[row][col];
                matrix[row][col] = matrix[col][row];
                matrix[col][row] = at_ij;
            }
        }
    }

    public Matrix multiply(Matrix other) {
        Matrix out = new Matrix(Arrays.copyOf(matrix, matrix.length));

        for (int row = 0; row < matrix.length; row++) {
            for (int col = 0; col < matrix[row].length; col++) {
                for (int otherRow = 0; otherRow < other.matrix.length; otherRow++) {
                    out.matrix[row][col] += matrix[row][col] * other.matrix[otherRow][col];
                }
            }
        }

        return out;
    }

    public void print() {
        for (int i = 0; i < matrix.length; i++) {
            System.out.println(Arrays.toString(matrix[i]));
        }
    }
}
