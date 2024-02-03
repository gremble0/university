import java.util.Arrays;

public class MatrixMultiplication {
    public static void main(String[] args) {
        int[][] matrix = {
            { 0,  1,  2,  3  },
            { 4,  5,  6,  7  },
            { 8,  9,  10, 11 },
            { 12, 13, 14, 15 },
        };

        for (int i = 0; i < matrix.length; i++) {
            System.out.println(Arrays.toString(matrix[i]));
        }
    }

    private static void rotateMatrix(int[][] matrix) {
    }
}
