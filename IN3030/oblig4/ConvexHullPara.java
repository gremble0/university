class ConvexHullPara extends ConvexHull {
  Thread[] threads;
  ConvexHullAroundLine[] tasks;

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
    tasks = new ConvexHullAroundLine[threads.length];
  }

  public IntList makeConvexHull() {
    int argMinX = argMin(x);
    int argMaxX = argMax(x);
    visited.add(argMinX);
    visited.add(argMaxX);

    IntList possibleStarts = new IntList();
    IntList possibleEnds = new IntList();
    possibleStarts.add(argMaxX);
    possibleEnds.add(argMinX);

    // Do a shallow BFS to find the starting points of each thread.
    // sqrt(threads.length) because the (fake) recursive depth will be increasing by
    // a factor of 2 for each iteration.
    for (int i = 0; i < Math.sqrt(threads.length); i++) {
      // Invariant: possibleStarts and possibleEnds are of the same size. could assert
      // but apparently this requires a compiler flag in java due to backwards
      // compatability co cba. Asserting could also increase runtime, which would be
      // undesirable.
      int halfSize = possibleStarts.size() / 2;
      IntList newStarts = new IntList();
      IntList newEnds = new IntList();
      for (int j = 0; j < halfSize; j++) {
        int startsAtJ = possibleStarts.get(j);
        int endsAtJ = possibleEnds.get(j);

        int furthestAbove = furthestAboveLine(startsAtJ, endsAtJ);
        if (furthestAbove == -1)
          continue;
        visited.add(furthestAbove);

        // Simulating the recursive calls inside the visitAbove method in the sequential
        // solution
        newStarts.add(furthestAbove);
        newEnds.add(endsAtJ);
        newStarts.add(startsAtJ);
        newEnds.add(furthestAbove);
      }

      for (int j = halfSize; j < possibleStarts.size(); j++) {
        int startsAtJ = possibleStarts.get(j);
        int endsAtJ = possibleEnds.get(j);

        int furthestBelow = furthestBelowLine(possibleStarts.get(j), possibleEnds.get(j));
        if (furthestBelow == -1)
          continue;
        visited.add(furthestBelow);

        // Simulating the recursive calls inside the visitBelow method in the sequential
        // solution
        newStarts.add(startsAtJ);
        newEnds.add(furthestBelow);
        newStarts.add(furthestBelow);
        newEnds.add(endsAtJ);
      }

      possibleStarts = newStarts;
      possibleEnds = newEnds;
    }

    int halfSize = possibleStarts.size() / 2;
    for (int i = 0; i < halfSize; i++) {
      tasks[i] = new ConvexHullAboveLine(possibleStarts.get(i), possibleEnds.get(i));
      threads[i] = new Thread(tasks[i]);
      threads[i].start();
    }

    for (int i = halfSize; i < possibleStarts.size(); i++) {
      tasks[i] = new ConvexHullBelowLine(possibleStarts.get(i), possibleEnds.get(i));
      threads[i] = new Thread(tasks[i]);
      threads[i].start();
    }

    try {
      // All threads may not be initialized if one of the branches in the BFS already
      // finished before starting the Runnable class, so only iterate up until
      // possibleStarts-/possibleEnds- .size()
      for (int i = 0; i < possibleStarts.size(); i++)
        threads[i].join();
    } catch (Exception e) {
      e.printStackTrace();
      return visited;
    }

    for (int i = 0; i < possibleStarts.size(); i++)
      visited.append(tasks[i].localVisited);

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

    long before = System.nanoTime();
    IntList hull = chs.makeConvexHull();
    long after = System.nanoTime();

    System.out.println((after - before) / 1000000);

    // Uncomment to draw graph for output. `hull` is not sorted so the graph does
    // not draw the actual hull, only all the points that we have determined to be
    // in the hull. If we wanted to draw the real hull we would have to sort the
    // hull returned by makeConvexHull.

    // Oblig4Precode precode = new Oblig4Precode(chs, hull);
    // precode.drawGraph();
  }
}
