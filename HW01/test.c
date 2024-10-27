int     _if();
int     _while();
double  _for();
int     _expression();
int     _sum(int a, int b);

int main() {
    int a = _if();
    int b = _while();
    double c = _for(6          ,5,7);
    double c = _for(6,43  , 7);
    int d = _expression()     ;
    int e = _sum(1, 2);

    printf("%d %d %f %d %d\n", a, b, c, d, e);
    return 0;
}

int _expression() {
    int a = 1;
    int b = 1 + 3;
    int c = 1 + 3 ;
    int     c    = 1 *       3    ;
    int d=1*3;
    return a + b + c + d;
}

int _while() {
    int a = 1;
    while (a == 1 || a == 2) {
        a = 2 *3;
    }
    while (a == 1 || a == 2) {
        a = 55 *3;
    }
    while (a == 1 || a == 2) {
        a = a - 1;
    }
    while (a == 1 || a == 2) {
        a = 56;
    }
    while (a == 1 || a == 2) {
        a = a + 1;
    }
    return a + 32;
}


int _if() {
    int a = 1;
    int b = 1 + 3;
    int c = 1 + 3 ;
    int     c    = 1 *       3    ;
    int d=1*3;

    if (a == 1 || a == 2) {
        a = 2 * 3;
    }
    if ( a == 3 || a == 4 ) {
        b = 2 * 3;
    }
    if (a==5||a==6){
        c = 2 * 3;
    }
    if (a != 1 || a >= 2 ) {
        d = 2 * 3;
    }
    if ( a == 1 || !(a == 2) ) {
        d = 2 * 3;
    }
    return 0;
}

int     _sum(int a, int b) {
    return a + b;
}

double  _for(int a, int b, int c) {
    double sum = 0;
    for (int i = a; i < b; i++) {
        sum = sum + i;
    }
    return sum;
}