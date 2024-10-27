int sum(int a, int b);

int sum(int a, int b) {
    return a + b;
}

int mult(int a, int b, int c) {
    return a + b * c;
}

int div(int a, int b) {
    return a / b;
}

int pow(int a, int b) {
    int result = 1;
    int i = 0;
    for (i = 0; i < b; i++) {
        result = result * a;
    }
    return result;
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
    int power = pow(2, 3);
    
    if (result > 25) {
        printf("Result is greater than 25\n");
        x = 5;
    }

    if ((x > 5 && x < 30) || y < 30) {
        printf("x is greater than 5 and y is less than 30\n");
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

    printf("Power: %d\n", power);

    return 0;
}
