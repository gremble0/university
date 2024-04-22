class ConvexHullPara extends ConvexHull {
  private class ConvexHullAboveLine implements Runnable {
    private int lineStart, lineEnd;
    public IntList visited;

    ConvexHullAboveLine(int lineStart, int lineEnd) {
      this.lineStart = lineStart;
      this.lineEnd = lineEnd;
    }

    public void run() {
    }
  }

  private class ConvexHullBelowLine implements Runnable {
    private int lineStart, lineEnd;
    public IntList visited;

    ConvexHullBelowLine(int lineStart, int lineEnd) {
      this.lineStart = lineStart;
      this.lineEnd = lineEnd;
    }

    public void run() {
    }
  }

  ConvexHullPara(int n, int seed) {
    super(n, seed);
  }

  public IntList makeConvexHull() {
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
