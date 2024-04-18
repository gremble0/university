class ConvexHullSeq extends ConvexHull {
  ConvexHullSeq(int n, int seed) {
    super(n, seed);
  }

  private void visitLine(int coord1, int coord2) {
    // TODO above param
    int furthestAbove = furthestAboveLine(coord1, coord2);
    if (furthestAbove != -1 && !visited.contains(furthestAbove)) {
      visited.add(furthestAbove);
      visitLine(coord1, furthestAbove);
    }

    // int furthestBelow = furthestBelowLine(coord1, coord2);
    // if (furthestBelow != -1 && !visited.contains(furthestBelow)) {
    // visited.add(furthestBelow);
    // visitLine(furthestBelow, coord2);
    // }
  }

  public IntList makeConvexHull() {
    int argMinX = argMin(x);
    int argMaxX = argMax(x);
    visited.add(argMinX);
    visited.add(argMaxX);

    visitLine(argMaxX, argMinX);

    // System.out.println("ddd: " + distanceFromLine(argMaxX, argMinX, 0) + ". " +
    // x[0] + "," + y[0]);

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

    ConvexHullSeq chs = new ConvexHullSeq(n, seed);
    IntList hull = chs.makeConvexHull();
    hull.print();

    Oblig4Precode precode = new Oblig4Precode(chs, hull);
    precode.drawGraph();
  }
}
