int expectf(float a, float b) {
    if (!(a == b)) {
        printf("Failed\n");
        printf("  %f expected, but got %f\n", a, b);
        exit(1);
    }
}

float  tf1(float a)  { return a; }

int main() {
    printf("Testing float ... ");

    expectf(1.0, 1.0);
    expectf(1.5, 1.0 + 0.5);
    expectf(0.5, 1.0 - 0.5);
    expectf(2.0, 1.0 * 2.0);
    expectf(0.25, 1.0 / 4.0);

    expectf(3.0, 1.0 + 2);
    expectf(2.5, 5 - 2.5);
    expectf(2.0, 1.0 * 2);
    expectf(0.25, 1.0 / 4);

    expectf(10.5, tf1(10.5));
    expectf(10.0, tf1(10));
    printf("OK\n");
    return 0;
}
