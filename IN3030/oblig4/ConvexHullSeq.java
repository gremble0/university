class ConvexHullSeq extends ConvexHull {
  ConvexHullSeq(int n, int seed) {
    super(n, seed);
  }

  private void visitAbove(int coord1, int coord2) {
    int furthestAbove = furthestAboveLine(coord1, coord2);
    // If furthestAboveLine returns -1 there are no more points above or on the line
    if (furthestAbove == -1)
      return;

    visited.add(furthestAbove);
    visitAbove(furthestAbove, coord2);
    visitAbove(coord1, furthestAbove);
  }

  private void visitBelow(int coord1, int coord2) {
    int furthestBelow = furthestBelowLine(coord1, coord2);
    if (furthestBelow == -1)
      return;

    visited.add(furthestBelow);
    visitBelow(coord1, furthestBelow);
    visitBelow(furthestBelow, coord2);
  }

  public IntList makeConvexHull() {
    int argMinX = argMin(x);
    int argMaxX = argMax(x);
    visited.add(argMinX);
    visited.add(argMaxX);

    visitAbove(argMaxX, argMinX);
    visitBelow(argMaxX, argMinX);

    return visited;
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
    IntList hull = chs.makeConvexHull();

    // Uncomment to draw graph for output. `hull` is not sorted so the graph does
    // not draw the actual hull, only all the points that we have determined to be
    // in the hull. If we wanted to draw the real hull we would have to sort the
    // hull returned by makeConvexHull.

    // Oblig4Precode precode = new Oblig4Precode(chs, hull);
    // precode.drawGraph();
  }
}
