class ConvexHullPara extends ConvexHull {
  Thread[] threads;

  private abstract class ConvexHullAroundLine implements Runnable {
    protected int lineStart, lineEnd;
    public IntList localVisited;

    protected ConvexHullAroundLine(int lineStart, int lineEnd) {
      this.lineStart = lineStart;
      this.lineEnd = lineEnd;
      this.localVisited = new IntList();
    }
  }

  private class ConvexHullAboveLine extends ConvexHullAroundLine {
    ConvexHullAboveLine(int lineStart, int lineEnd) {
      super(lineStart, lineEnd);
    }

    public void run() {
      visitAbove(lineStart, lineEnd);
    }

    private void visitAbove(int coord1, int coord2) {
      int furthestAbove = furthestAboveLine(coord1, coord2);
      if (furthestAbove == -1)
        return;

      localVisited.add(furthestAbove);
      visitAbove(furthestAbove, coord2);
      visitAbove(coord1, furthestAbove);
    }
  }

  private class ConvexHullBelowLine extends ConvexHullAroundLine {
    ConvexHullBelowLine(int lineStart, int lineEnd) {
      super(lineStart, lineEnd);
    }

    public void run() {
      visitBelow(lineStart, lineEnd);
    }

    private void visitBelow(int coord1, int coord2) {
      int furthestBelow = furthestBelowLine(coord1, coord2);
      if (furthestBelow == -1)
        return;

      localVisited.add(furthestBelow);
      visitBelow(coord1, furthestBelow);
      visitBelow(furthestBelow, coord2);
    }
  }

  ConvexHullPara(int n, int seed) {
    super(n, seed);
    threads = new Thread[Runtime.getRuntime().availableProcessors()];
  }

  public IntList makeConvexHull() {
    int argMinX = argMin(x);
    int argMaxX = argMax(x);
    visited.add(argMinX);
    visited.add(argMaxX);

    // int aboveStart = argMaxX;
    // int aboveEnd = argMinX;
    // int belowStart = argMaxX;
    // int belowEnd = argMinX;
    // int furthestBelow = furthestBelowLine(argMaxX, argMinX);
    // int furthestAbove = furthestAboveLine(argMaxX, argMinX);
    // IntList threadStarts = new IntList();
    // IntList threadEnds = new IntList();
    // for (int i = 0; i < threads.length; i++) {
    // }
    ConvexHullAroundLine[] tasks = new ConvexHullAroundLine[2];
    tasks[0] = new ConvexHullBelowLine(argMaxX, argMinX);
    tasks[1] = new ConvexHullAboveLine(argMaxX, argMinX);
    threads[0] = new Thread(tasks[0]);
    threads[1] = new Thread(tasks[1]);
    threads[0].start();
    threads[1].start();

    try {
      threads[0].join();
      threads[1].join();
    } catch (Exception e) {
    }

    visited.append(tasks[0].localVisited);
    visited.append(tasks[1].localVisited);

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

    ConvexHull chs = new ConvexHullPara(n, seed);
    IntList hull = chs.makeConvexHull();

    Oblig4Precode precode = new Oblig4Precode(chs, hull);
    precode.drawGraph();
  }
}
