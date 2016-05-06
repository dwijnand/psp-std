package psp.api;

public enum Cmp {
  LT(-1), EQ(0), GT(1);

  private final int intValue;
  private Cmp(final int intValue) { this.intValue = intValue; }
  public int intValue() { return intValue; }
  public Cmp flip() {
    if (intValue < 0) return GT;
    else if (intValue > 0) return LT;
    return EQ;
  }
}
