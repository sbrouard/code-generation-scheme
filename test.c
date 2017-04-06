#include <stdio.h>
#include <stdlib.h>



int exp7(int x){
int x1 = x * 1;
int x2 = x1 * x1;
int x3 = x1 * x2;
x1 = x2 * x3;
int x7 = x2 * x1;
return x7;
}

int main(){
    printf("2 exposant 7 = %d\n", exp7(2)); //2⁷ = 128
    return 0;
}
