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

  protected int furthestNegativeBetween(int coord1, int coord2) {
    int xBetween = (x[coord2] + x[coord1]) / 2;
    int yBetween = (y[coord2] + y[coord1]) / 2;

    int furthest = 0;
    int furthestI = 0;
    for (int i = 0; i < n; i++) {
      // Euclidian distance
      int distance = ((x[xBetween] - x[i]) * (x[xBetween] - x[i]))
          + ((y[yBetween] - y[i]) * (y[yBetween] - y[i]));

      if (distance > furthest) { // >= ?
        furthest = distance;
        furthestI = i;
      }
    }

    return furthestI;
  }

  protected int furthestPositiveBetween(int coord1, int coord2) {
    int xBetween = (x[coord2] + x[coord1]) / 2;
    int yBetween = (y[coord2] + y[coord1]) / 2;

    double furthest = 0;
    int furthestI = 0;
    for (int i = 0; i < n; i++) {
      // Euclidian distance
      double distance = Math.pow(x[xBetween] - x[i], 2) + Math.pow(y[yBetween] - y[i], 2);

      if (distance > furthest) { // >= ?
        furthest = distance;
        furthestI = i;
      }
    }

    return furthestI;
  }

  // protected Coordinate furthestPositiveFrom(Coordinate c1, Coordinate c2) {
  // Coordinate betweenC1C2 = new Coordinate(c2.x - c1.x, c2.y - c1.y);
  //
  // double furthest = 0;
  // int furthestI = 0;
  // for (int i = 0; i < n; i++) {
  // Coordinate iCoordinate = new Coordinate(x[i], y[i]);
  //
  // // Euclidian distance
  // double distance = Math.pow(betweenC1C2.x - iCoordinate.x, 2) +
  // Math.pow(betweenC1C2.y - iCoordinate.y, 2);
  //
  // if (distance < furthest) {
  // furthest = distance;
  // furthestI = i;
  // }
  // }
  //
  // visited.add(furthestI);
  //
  // return new Coordinate(x[furthestI], y[furthestI]);
  // }

  abstract public IntList makeConvexHull();
}
