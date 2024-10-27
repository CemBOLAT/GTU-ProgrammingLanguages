int sum(int a, int b);

int sum(int a, int b) {
    return a + b;
}

int mult(int a, int b, int c) {
    return a * b * c;
}

int print_test() {
    printf("Hello World\n");
    return 0;
}

int main() {
    int x = 10;
    int y = 20;
    int i = 0;
    int result = sum(x, y);
    
    if (result > 25) {
        printf("Result is greater than 25\n");
        x = 5;
    }

    for (i = 0; i < 10; i++) {
        printf("%d\n", i);
    }

    x = mult(x, y, result);
    printf("Result: %d\n", x);

    x = x + 1;

    while (x > 600) {
        printf("%d\n", x);
        x = x / 2;
    } 

    print_test();

    return 0;
}
