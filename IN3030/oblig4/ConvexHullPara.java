class ConvexHullPara extends ConvexHull {
  ConvexHullPara(int n, int seed) {
    super(n, seed);
  }

  public IntList makeConvexHull() {
    return visited;
  }
}
