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
    threads = new Thread[4];
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
      // compatability so cba. Asserting could also increase runtime, which would be
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

    // At the moment some of the localVisited contains duplicates, which means
    // something is going wrong.
    for (int i = 0; i < possibleStarts.size(); i++)
      visited.append(tasks[i].localVisited);

    return visited;
  }

  @Override
  public String toString() {
    return "ConvexHullPara";
  }
}
