#include<stdio.h>

int main(){
    int inteiro = 100;
    int atual = 2;

    if(inteiro > 1){
        while(atual <= inteiro){
            int i = 0;
            int isPrime = 1;
            
            for(i=2; i < atual; i++){
                if(atual % i == 0){
                    isPrime = 0;
                    break;
                }
            }

            if(isPrime){
                printf("%d\n", atual);
            }

            atual += 1;
        }
    }
    return 0;
}