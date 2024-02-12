public class TestMatrix {
    public static void main(String[] args) {
        testForN(100);
        testForN(200);
        testForN(500);
        testForN(1000);
    }

    private static void testForN(final int n) {
        final int seed = 42;

        Matrix a = new Matrix(Oblig2Precode.generateMatrixA(seed, n));
        Matrix b = new Matrix(Oblig2Precode.generateMatrixB(seed, n));

        System.out.println("-------TESTING FOR n=" + n + "-------");
        
        // Sequential
        long beforeSnt = System.nanoTime();
        Matrix snt = a.multiply(b, Oblig2Precode.Mode.SEQ_NOT_TRANSPOSED);
        long afterSnt = System.nanoTime();

        long beforeSat = System.nanoTime();
        Matrix sat = a.multiply(b, Oblig2Precode.Mode.SEQ_A_TRANSPOSED);
        long afterSat = System.nanoTime();

        long beforeSbt = System.nanoTime();
        Matrix sbt = a.multiply(b, Oblig2Precode.Mode.SEQ_B_TRANSPOSED);
        long afterSbt = System.nanoTime();

        for (int i = 0; i < a.matrix.length; i++) {
            for (int j = 0; j < a.matrix[i].length; j++) {
                if (snt.matrix[i][j] != sat.matrix[i][j]) {
                    System.out.println("Found diffs at matrix[" + i + "][" + j + "]: ");
                    System.out.println("SEQ_NOT_TRANSPOSED: " + snt.matrix[i][j]);
                    System.out.println("SEQ_A_TRANSPOSED:   " + sat.matrix[i][j]);
                    System.out.println("SEQ_B_TRANSPOSED:   " + sbt.matrix[i][j]);
                }
            }
        }

        System.out.println("SEQ_NOT_TRANSPOSED: " + (afterSnt - beforeSnt));
        System.out.println("SEQ_A_TRANSPOSED:   " + (afterSat - beforeSat));
        System.out.println("SEQ_B_TRANSPOSED:   " + (afterSbt - beforeSbt));

        // Parallel
        // Matrix f = a.multiply(b, Oblig2Precode.Mode.PARA_NOT_TRANSPOSED);
        // Matrix g = a.multiply(b, Oblig2Precode.Mode.PARA_A_TRANSPOSED);
        // Matrix h = a.multiply(b, Oblig2Precode.Mode.PARA_B_TRANSPOSED);
    }
}
