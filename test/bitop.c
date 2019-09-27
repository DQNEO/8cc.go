void test_or() {
    expect(3, 1 | 2);
    expect(7, 2 | 5);
    expect(7, 2 | 7);
}

void test_and() {
    expect(0, 1 & 2);
    expect(2, 2 & 7);
}

int main() {
    printf("Testing bitwise operators ... ");

    test_or();
    test_and();

    printf("OK\n");
    return 0;
}
