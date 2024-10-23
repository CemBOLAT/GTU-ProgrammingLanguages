int     _if();
int     _while();
double  _for();
int     _expression();

int main() {
    int a = _if();
    int b = _while(3);
    double c = _for(6          ,5,7);
    double c = _for(6,5  , 7);
    int d = _expression()     ;

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
        return 1;
    }
    if ( a == 3 || a == 4 ) {
        return 5;
    }
    if(a==5||a==6){
        return 6;
    }
    if (a != 1 || a >= 2 ) {
        return 1;
    }
    if ( a == 1 || !(a == 2) ) {
        return 1;
    }
    return 0;
}