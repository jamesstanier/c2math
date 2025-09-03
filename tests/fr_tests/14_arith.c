int arith(int a, int b, int c) {
    // precedence: b*2 happens before a + (...) and before (a/b)
    return a + (b * 2 - (a / b)) * c;
}
