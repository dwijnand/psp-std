package psp.std;

public enum Cmp {
  LT(-1), EQ(0), GT(1);

  private final int value;
  private Cmp(final int value) { this.value = value; }
  public int intValue() { return value; }
  public Cmp flip() {
    if (value < 0) return GT;
    else if (value > 0) return LT;
    return EQ;
  }
}
