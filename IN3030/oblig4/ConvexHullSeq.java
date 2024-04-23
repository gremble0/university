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

  @Override
  public String toString() {
    return "ConvexHullSeq";
  }
}
