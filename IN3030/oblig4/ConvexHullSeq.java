import java.util.Arrays;

class ConvexHullSeq extends ConvexHull {
  ConvexHullSeq(int n, int seed) {
    super(n, seed);
  }

  public int[] makeConvexHull() {
    System.out.println(Arrays.toString(x));
    System.out.println(Arrays.toString(y));

    System.out.println(argMax(x));
    System.out.println(argMin(x));
    return x;
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
    chs.makeConvexHull();
  }
}
