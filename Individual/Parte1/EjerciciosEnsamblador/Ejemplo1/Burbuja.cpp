#include <stdlib.h>
#include <stdio.h>


void bubbleSort(int arr[], int n);
void swapNumber(int *a, int *b);

int main(void)
{
    const int arrSize = 20;
    
    int byteCount = sizeof(int) * arrSize;

    int* arr = (int*)_aligned_malloc(byteCount, 16);
    if (arr == NULL)
        return 1;

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

void bubbleSort(int* arr, int size)
{
    int* a = &size;

    __asm {
            mov esi, arr;       // Inicio del array.
            mov edi, a;         // tamaño.
            // --------------------------------

         /*   mov eax, edi;       // contador externo.
        
        externo:                // Tag del bucle externo.
            
            mov ebx, edi;       // contador interno.
            sub ebx, eax;

        interno:                // Tag del bucle interno.*/
            mov ecx, [esi];     // Valor actual.
            mov edx, [esi + 4]; // Valor siguiente.
            // cpm ecx, edx;       // Comparamos el valor actual con el siguiente
            // jl guardar;         // Si el valor actual ECX es menor que la siguiente [ESI + 4], entonces nos saltamos el paso de intercambiarlos.

            xchg ecx, edx;      // Intercambiamos.

            mov [esi], ecx;     // Guardamos el valor actual.
            mov [esi + 4], edx; // Guardamos el valor siguiente.

     /*   guardar:                // Tag  de las operaciones comunes por iteracion
            mov esi, [esi + 4]  // Siguiente iteracion
            dec ebx;            // Decretemntamos 1 en el contador interno.
            jnz interno;        // Si no es 0, repetimos el bucle interno.

            dec eax;            // Decrementamos 1 en el contador externo.
            jnz externo;        // Si no es 0, repetimos el bucle externo.*/
    }

    /*
     for (int i = 0; i < size - 1; i++)
        for (int j = 0; j < size - i - 1; j++)
            if (arr[j] > arr[j + 1]) {

                printf_s("%d", arr[j]);
                printf_s(" = ");

                swapNumber(&arr[j], &arr[j+1]);

                printf_s("%d \n", arr[j]);
              
            }*/
            
}

void swapNumber(int* a, int* b /*, int* size*/ ) {
    __asm
    {
        // Haciendo que las variables de nustro codigo en C sea usable en nuestro bloque __asm.
        mov esi, a;        // guardamos las referencias de las variables en un registro.
        mov edi, b;        // guardamos las referencias de las variables en un registro.

        // Empieza la manipulacion
        mov eax, [esi];    // guardamos los valores en memoria de las referencias en un registro.
        mov ebx, [edi];    // guardamos los valores en memoria de las referencias en un registro.

        xchg eax, ebx;     // damos la vuelta a los valores que tenemos guardados en los registros.

        mov [esi], eax;    // guardamos los valores de los registros de vuelta a la memoria.
        mov [edi], ebx;    // guardamos los valores de los registros de vuelta a la memoria.

    }
}