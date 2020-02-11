#include <stdlib.h>
#include <stdio.h>


void bubbleSort(int arr[], int n);
void swapNumber(int *a, int *b);

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
     for (int i = 0; i < size - 1; i++)
        for (int j = 0; j < size - i - 1; j++)
            if (arr[j] > arr[j + 1]) {

                printf_s("%d", arr[j]);
                printf_s(" = ");

                swapNumber(&arr[j], &arr[j+1]);

                printf_s("%d \n", arr[j]);
                
                /*
                int tempVal = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tempVal;*/
            }
}

void swapNumber(int* a, int* b /*, int* size*/ ) {
    __asm
    {
 /*
    outterloop:
        add edx, 1;     // Counter i ;
        move ebs, edx;  // move counter to countercalculation site.
        add ebs, -size; // i - size;
        add ebs, -1;    // i - 1;

        // Condition to jump if i edx >= efx
        jz innerloop;

    innerloop:
        add eex, 1;     // Counter j;
        move ebs, eex;  // move counter to countercalculation site.
        add ebs, -size; // i - size;
        add ebs, -edx;  // i - i;
        add ebs, -1;    // i - 1;

        // Condition to jump if eex >= efx
        jz end;

    innerswap:

        // check if arr[j] > arr[j + 1], and if its, do swap.

        jz swap;
    swap:
*/

        // Tenemos que traspasar la direccion de MEMORIA de las variables
        //  a los REGISTROS, y una vez ahi, usamos los corchetes para hacer referencia a
        //  a la direccion de memoria.
        mov esi, a;
        mov edi, b;

        mov eax, [esi];
        mov edx, [edi];

        mov [esi], edx;
        mov [edi], eax;

        /* NO FUNCIONA
        mov eax, a;
        mov ebx, b;

        mov ecx, eax;
        mov a, ebx;
        mov b, ecx;
        */
       
 // end:

    }
    
}