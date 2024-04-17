abstract class ConvexHull {
  int n, MAX_X, MAX_Y;
  int x[], y[];

  public ConvexHull(int n, int seed) {
    this.x = new int[n];
    this.y = new int[n];
    new NPunkter(n, seed).fyllArrayer(x, y);
  }

  protected record CoordinateArgs(int xi, int yi) {
    public CoordinateArgs(int xi, int yi) {
      this.xi = xi;
      this.yi = yi;
    }
  }

  protected static int argMin(int[] arr) {
    int min = 0;
    for (int i = 0; i < arr.length; i++)
      if (arr[i] < arr[min])
        min = i;

    return min;
  }

  protected static int argMax(int[] arr) {
    int max = 0;
    for (int i = 0; i < arr.length; i++)
      if (arr[i] > arr[max])
        max = i;

    return max;
  }

  protected static int argFurthestNegative(CoordinateArgs c1, CoordinateArgs c2) {
    return 0;
  }

  protected static int argFurthestPositive(CoordinateArgs c1, CoordinateArgs c2) {
    return 0;
  }

  abstract public int[] makeConvexHull();
}
