#include <stdlib.h>
#include <stdio.h>


void bubbleSort(int arr[], int n);

int main(void)
{
    const int arrSize = 20;
    int arr[arrSize];
    
    srand(27015);

    printf_s("Valor del array desordenado: \n");
    for (size_t i = 0; i < arrSize; i++)
    {
        arr[i] = rand();

        if (i + 1 != arrSize)
            printf_s("%d, ", arr[i]);
        else
            printf_s("%d \n\n", arr[i]);
    }

    bubbleSort(arr, arrSize);

    printf_s("Valor del array ordenado: \n");
    for (size_t i = 0; i < arrSize; i++)
    {
        if (i+1 != arrSize)
            printf_s("%d, ", arr[i]);
        else
            printf_s("%d \n", arr[i]);
    }

}

void bubbleSort(int arr[], int size)
{
     for (size_t i = 0; i < size - 1; i++)
        for (size_t j = 0; j < size - i - 1; j++)
            if (arr[j] > arr[j + 1]) {

                /*__asm
                {
                    mov eax, arr[j]
                    mov ebx, arr[j + 1]
                }


                __asm
                {
                    mov dx, count
                    oloop :
                    mov cx, count
                        lea si, nums

                        iloop :
                    mov al, [si]; Because compare can't have both memory
                        cmp al, [si + 1]
                        jl common; if al is less than[si + 1] Skip the below two lines for swapping.
                        xchg al, [si + 1]
                        mov[si], al; Coz we can't use two memory locations in xchg directly.

                        common:
                    INC si
                        loop iloop

                        dec dx
                        jnz oloop
                }*/


                int tempVal = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tempVal;
            }
}


