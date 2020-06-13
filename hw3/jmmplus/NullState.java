class NullState implements State {
    private long[] value;
    NullState(int length) { value = new long[length]; }
    public int size() { return value.length; }
    public long[] current() { return value; }
    public void swap(int i, int j) { }
}
