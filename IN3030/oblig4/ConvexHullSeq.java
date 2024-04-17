class ConvexHullSeq extends ConvexHull {
  ConvexHullSeq(int n, int seed) {
    super(n, seed);
  }

  private void visitCoord(int coord) {
    int argMinX = argMin(x);
    int argMaxX = argMax(x);
    visited.add(argMinX);
    visited.add(argMaxX);

    visited.add(furthestNegativeBetween(argMinX, argMaxX));
    visited.add(furthestPositiveBetween(argMinX, argMaxX));
  }

  public IntList makeConvexHull() {
    int argMinX = argMin(x);
    int argMaxX = argMax(x);
    visited.add(argMinX);
    visited.add(argMaxX);

    visited.add(furthestNegativeBetween(argMinX, argMaxX));
    // visited.add(furthestPositiveBetween(argMinX, argMaxX));

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
