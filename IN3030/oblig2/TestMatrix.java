public class TestMatrix {
    public static void main(String[] args) {
	Matrix a = new Matrix(Oblig2Precode.generateMatrixA(42, 4));

	a.print();
	System.out.println();
	Matrix c = a.multiply(a, Oblig2Precode.Mode.SEQ_NOT_TRANSPOSED);
	c.print();
	// testForN(100);
	// testForN(200);
	// testForN(500);
	// testForN(1000);
    }

    private static void testForN(final int n) {
	final int seed = 42;

	Matrix a = new Matrix(Oblig2Precode.generateMatrixA(seed, n));
	Matrix b = new Matrix(Oblig2Precode.generateMatrixB(seed, n));

	// Sequential
	Matrix c = a.multiply(b, Oblig2Precode.Mode.SEQ_NOT_TRANSPOSED);
	Matrix d = a.multiply(b, Oblig2Precode.Mode.SEQ_A_TRANSPOSED);
	Matrix e = a.multiply(b, Oblig2Precode.Mode.SEQ_B_TRANSPOSED);

	// Parallel
	// Matrix f = a.multiply(b, Oblig2Precode.Mode.PARA_NOT_TRANSPOSED);
	// Matrix g = a.multiply(b, Oblig2Precode.Mode.PARA_A_TRANSPOSED);
	// Matrix h = a.multiply(b, Oblig2Precode.Mode.PARA_B_TRANSPOSED);
    }
}
