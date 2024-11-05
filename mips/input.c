int sum( int a, int b);



int sum(int a, int b) {
    return a + b;
}

int main() {
    int x = 10;
    int y = 20 + 5 * 98;
    int result = sum(x, y);

    x = x + 5 * y;

    if (result != 25){
        printf("Result is greater than 25\n");
    } 
    if (result < 25) {
        printf("Result is 25 o less\n");
    }

    for (int i = 0 ; i < 10; i++) {
        printf("%d\n", i);
    }

    

    while (6 == 7);

    while (8 >= 67) {
        float local_var = 9;
        sum(local_var, 78);
    }

    for(int i = 0 ; i < 5; i++);

    sum(x,y);

    return 0;
}

