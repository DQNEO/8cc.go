int expect(int a, int b) {
    if (!(a == b)) {
        printf("Failed\n");
        printf("  %d expected, but got %d\n", a, b);
        exit(1);
    }
}

#foo bar

int main() {
    printf("Testing macros ... ");

    expect(1, 1);
    expect(2, 2);

    printf("OK\n");
    return 0;
}
