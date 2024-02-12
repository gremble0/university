namespace Matrixes
{
    class Matrix
    {
        private int[][] values { get; }

        public Matrix(int[][] values)
        {
            this.values = values;
        }
        
        override public string ToString()
        {
            string result = "";

            foreach (int[] row in values)
            {
                result += "[ ";
                foreach (int value in row)
                {
                    result += value + ", ";
                }
                result += "]\n";
            }

            return result;
        }

        public void transpose()
        {
            for (int row = 0; row < values.Length; ++row)
            {
                for (int col = row; col < values[row].Length; ++col)
                {
                    int temp = values[row][col];
                    values[row][col] = values[col][row];
                    values[col][row] = temp;
                }
            }
        }

        public Matrix multiply(Matrix other)
        {
            int[][] resultValues = new int[values.Length][];

            for (int row = 0; row < values.Length; ++row)
            {
                for (int col = 0; col < values[col].Length; ++col)
                {
                    for
                }
            }

            Matrix result = new Matrix(resultValues);
            return result;
        }
    }

    class TestMatrix
    {
        static void Main(string[] args)
        {
            Matrix m = new Matrix(new int[][] {
                new int[] { 1, 2, 3 },
                new int[] { 4, 5, 6 },
                new int[] { 7, 8, 9 },
            });

            Console.WriteLine(m);
            m.transpose();
            Console.WriteLine(m);
        }
    }
}
