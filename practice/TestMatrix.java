public class TestMatrix {
    public static void main(String[] args) {
        Matrix matrix = new Matrix(new int[][]{
            { 0,  1,  2,  3  },
            { 4,  5,  6,  7  },
            { 8,  9,  10, 11 },
            { 12, 13, 14, 15 },
        });
        Matrix matrix2 = new Matrix(new int[][]{
            { 0,  1,  2,  3  },
            { 4,  5,  6,  7  },
            { 8,  9,  10, 11 },
            { 12, 13, 14, 15 },
        });

        matrix.print();
        matrix.transpose();
        System.out.println();
        matrix.print();

        System.out.println();
        Matrix matrix3 = matrix.multiply(matrix2);
        matrix3.print();
    }
}
