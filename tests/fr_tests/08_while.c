int sum_to_n(int n) {
    int s = 0;
    int i = 0;
    while (i < n) {
        s += i;
        i++;
    }
    return s;
}
