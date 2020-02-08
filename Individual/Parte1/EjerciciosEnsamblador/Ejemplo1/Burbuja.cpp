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
                swapNumber(&arr[j], &arr[j + 1]);
                printf_s("%d \n", arr[j]);
                /*
                int tempVal = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tempVal;*/
            }
}

void swapNumber(int *a, int* b) {
    __asm
    {
        mov eax, a;
        mov ebx, b;

        mov ecx, eax;
        mov a, ebx;
        mov b, ecx;
    }
}


