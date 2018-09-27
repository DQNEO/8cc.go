enum { g1, g2, g3 } global1;

int main() {
    printf("Testing enum ... ");

    expect(0, g1);
    expect(2, g3);

    enum { x } v;
    expect(0, x);

    printf("OK\n");
    return 0;
}
