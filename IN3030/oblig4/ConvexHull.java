abstract class ConvexHull {
  int n, MAX_X, MAX_Y;
  int x[], y[];
  IntList visited;

  public ConvexHull(int n, int seed) {
    this.n = n;
    this.x = new int[n];
    this.y = new int[n];
    new NPunkter(n, seed).fyllArrayer(x, y);
    this.MAX_X = argMax(x);
    this.MAX_Y = argMax(y);

    this.visited = new IntList();
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

  // These geometry functions are mostly just copy pasted and probably
  // unnecessarily overcomplicated, but they work so I have not looked into better
  // ways of doing this.
  protected boolean pointIsAboveLine(int lineStart, int lineEnd, int point) {
    // int x1 = x[lineStart];
    // int x2 = x[lineEnd];
    // int y1 = y[lineStart];
    // int y2 = y[lineEnd];
    // int xP = x[point];
    // int yP = y[point];

    double m = (double) (y[lineEnd] - y[lineStart]) / (x[lineEnd] - x[lineStart]);
    double b = y[lineStart] - m * x[lineStart];
    double yLine = m * x[point] + b;

    return y[point] > yLine;
  }

  public double distanceFromLine(int lineStart, int lineEnd, int point) {
    int x1 = x[lineStart];
    int x2 = x[lineEnd];
    int y1 = y[lineStart];
    int y2 = y[lineEnd];
    int xP = x[point];
    int yP = y[point];

    int numerator = Math.abs((y2 - y1) * xP - (x2 - x1) * yP + x2 * y1 - y2 * x1);
    double denominator = Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1));

    return numerator / denominator;
  }

  // protected int furthestNegativeBetween(int coord1, int coord2) {
  // double furthest = 0;
  // int furthestI = 0;
  // for (int i = 0; i < n; i++) {
  // if (!pointIsAboveLine(coord1, coord2, i) || visited.contains(i))
  // continue;
  //
  // double distance = distanceFromLine(coord1, coord2, i);
  //
  // if (distance > furthest) { // >= ?
  // furthest = distance;
  // furthestI = i;
  // }
  // }
  //
  // return furthestI;
  // }

  /**
   * Get the index into `x` and y of the point furthest away from the line drawn
   * between two points. Get the furthest above if `above` is true and furthest
   * below if `above` is false.
   *
   * @param coord1 index into `x` and y of the first point
   * @param coord2 index into `x` and y of the second point
   * @param above  whether to get the furthest point above or below the line
   * @return index into `x` and y
   */
  protected int furthestBetweenLineInDirection(int coord1, int coord2, boolean above) {
    double furthest = 0;
    int furthestI = 0;
    for (int i = 0; i < n; i++) {
      boolean isAbove = pointIsAboveLine(coord1, coord2, i);
      if (above ? isAbove : !isAbove || visited.contains(i))
        continue;

      double distance = distanceFromLine(coord1, coord2, i);

      if (distance > furthest) { // >= ?
        furthest = distance;
        furthestI = i;
      }
    }

    return furthestI;
  }

  abstract public IntList makeConvexHull();
}
