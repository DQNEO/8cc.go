extern int expect(int, int);
extern int externvar1;

int main() {
    expect(98, externvar1);
}
