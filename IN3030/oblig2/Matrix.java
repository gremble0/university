import java.util.Arrays;

public class Matrix {
    private double[][] matrix;

    public Matrix(double[][] matrix) {
        this.matrix = matrix;
    }

    public Matrix transpose() {
	Matrix transposed = new Matrix(this.matrix);

        for (int row = 0; row < matrix.length; row++) {
            for (int col = row; col < matrix[row].length; col++) {
		transposed.matrix[row][col] = matrix[col][row];
		transposed.matrix[col][row] = matrix[row][col];
                // double at_ij = matrix[row][col];
                // matrix[row][col] = matrix[col][row];
                // matrix[col][row] = at_ij;
            }
        }

	return transposed;
    }

    public Matrix multiply(Matrix other, Oblig2Precode.Mode mode) {
	switch (mode) {
	    case SEQ_NOT_TRANSPOSED:
		return multiplySequential(other);

	    case SEQ_A_TRANSPOSED:
		Matrix this_transposed = transpose();
		return this_transposed.multiplySequential(other);

	    case SEQ_B_TRANSPOSED:
		Matrix other_transposed = other.transpose();
		return multiplySequential(other_transposed);

	    case PARA_NOT_TRANSPOSED:
		return multiplyParallel(other);

	    case PARA_A_TRANSPOSED:
		Matrix this_transposed = transpose();
		return this_transposed.multiplyParallel(other);

	    case PARA_B_TRANSPOSED:
		Matrix other_transposed = other.transpose();
		return multiplyParallel(other_transposed);

	    default:
		throw new EnumConstantNotPresentException(mode.getClass(), mode.getClass().getName());
	}
    }

	private Matrix multiplySequential(Matrix other) {
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

	private Matrix multiplyParallel(Matrix other) {
		return other;
	}

    public void print() {
        for (int i = 0; i < matrix.length; i++) {
            System.out.println(Arrays.toString(matrix[i]));
        }
    }
}
