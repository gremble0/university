import java.util.Arrays;

abstract class ConvexHull {
  static final int N_TEST_RUNS = 7;
  int n, MAX_X, MAX_Y;
  int x[], y[];
  IntList visited;

  protected ConvexHull(int n, int seed) {
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

  private double distanceFromLine(int lineStart, int lineEnd, int point) {
    int x1 = x[lineStart];
    int x2 = x[lineEnd];
    int y1 = y[lineStart];
    int y2 = y[lineEnd];
    int xP = x[point];
    int yP = y[point];

    int numerator = (y2 - y1) * xP - (x2 - x1) * yP + x2 * y1 - y2 * x1;
    double denominator = Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1));

    return numerator / denominator;
  }

  /**
   * Get the index into `x` and `y` of the point furthest below the line drawn
   * between two points given by the parameters to the method.
   *
   * @param coord1 index into `x` and `y` of the first point
   * @param coord2 index into `x` and `y` of the second point
   * @return index into `x` and `y`
   */
  protected int furthestBelowLine(int coord1, int coord2) {
    double furthest = 0;
    int furthestI = -1;
    for (int i = 0; i < n; i++) {
      double distance = distanceFromLine(coord1, coord2, i);
      if (distance > 0)
        continue;

      // If distance == 0 we are comparing a point that is on the line, which would be
      // fine for both above and below line

      if (distance < furthest) {
        furthest = distance;
        furthestI = i;
      }
    }

    return furthestI;
  }

  protected int furthestAboveLine(int coord1, int coord2) {
    double furthest = 0;
    int furthestI = -1;
    for (int i = 0; i < n; i++) {
      double distance = distanceFromLine(coord1, coord2, i);

      if (distance < 0)
        continue;

      if (distance > furthest) {
        furthest = distance;
        furthestI = i;
      }
    }

    return furthestI;
  }

  private static void benchmarkSubclass(ConvexHull hull) {
    // Run algorithm `N_TEST_RUNS` times and report median runtime
    long[] times = new long[N_TEST_RUNS];
    for (int i = 0; i < N_TEST_RUNS; i++) {
      long before = System.nanoTime();
      hull.makeConvexHull();
      long after = System.nanoTime();

      times[i] = after - before;

      // Cleanup before next usage
      hull.visited.clear();
    }

    Arrays.sort(times);
    long median = times[N_TEST_RUNS / 2];

    System.out.println(hull + " median time over " + N_TEST_RUNS + " test runs: " + median / 1000000 + "ms");
  }

  public static void main(String[] args) {
    int n, seed;
    try {
      if (args.length != 2)
        throw new Exception("Program takes 2 arguments <n: int> <seed: int>");

      n = Integer.parseInt(args[0]);
      seed = Integer.parseInt(args[1]);
    } catch (Exception e) {
      e.printStackTrace();
      return;
    }

    ConvexHull chs = new ConvexHullSeq(n, seed);
    ConvexHull chp = new ConvexHullPara(n, seed);

    benchmarkSubclass(chs);
    benchmarkSubclass(chp);

    // Uncomment to draw graph for output and write to file. `hull` is not sorted so
    // the graph does not draw the actual hull, only all the points that we have
    // determined to be in the hull. If we wanted to draw the real hull we would
    // have to sort the hull returned by makeConvexHull.

    // Oblig4Precode precode = new Oblig4Precode(chp, chp.makeConvexHull());
    // precode.writeHullPoints();
    // precode.drawGraph();
  }

  abstract public IntList makeConvexHull();
}
