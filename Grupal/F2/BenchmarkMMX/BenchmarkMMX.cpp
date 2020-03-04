#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <chrono>
#include <fstream>
#include <string.h>
#include "malloc.h"

#ifdef _WIN32
#include <windows.h>

double PCFreq = 0.0;
__int64 CounterStart = 0;

void StartCounter()
{
    LARGE_INTEGER li;
    if (!QueryPerformanceFrequency(&li))
        printf("QueryPerformanceFrequency failed!\n");

    PCFreq = double(li.QuadPart);

    QueryPerformanceCounter(&li);
    CounterStart = li.QuadPart;
}

double GetCounter()
{
    LARGE_INTEGER li;
    QueryPerformanceCounter(&li);
    return double(li.QuadPart - CounterStart) / PCFreq;
}
#endif

// Rellena los arrays con valores ascendentes dependiendo del tamaño del propio array.
void rellenar(int* vector1, int* vector2, int size) {
    for (int index = 0; index < size; index++) {
        vector1[index] = (index);
        vector2[index] = (index);
    };
};

// Multiplica los arrays.
void MMXmultiplicar(int* vector1, int* vector2, int size) {
    int* tam = &size;
    int* resMult = 0;

    __asm {
        mov esi, 0;                        // esi será el índice para acceder a los datos
        mov eax, 0;                        // eax será donde guardaremos la multiplicacion
        mov ebx, tam;                      // contador
        mov ecx, 0;                        // vector1[i]
        mov edx, 0;                        // vector2[i]
    loop_multiplicar:
        mov ecx, [vector1 + esi * 4];
        mov eax, [ecx];
        mov edx, [vector2 + esi * 4];
        imul edx, eax;
        add resMult, edx
        inc esi;                           // porque cada elemento ocupa 4 bytes.
        cmp esi, ebx;                      // comparamos con 6 porque es el índice del primer elemento fuera del vector.
        jl loop_multiplicar;               // recorremos la matriz con un solo índice para sumarla
        mov esi, 0;                        // esi será el índice para acceder a los datos
    };
};

// Suma los arrays.
void MMXsumar(int* vector1, int* vector2, int size) {
    int* tam = &size;
    int* resSuma = 0;

    __asm {
        // recorremos el vector para multiplicarlo
        mov esi, 0;                        // esi será el índice para acceder a los datos
        mov eax, 0;                        // eax será donde guardaremos la multiplicacion
        mov ebx, tam;                     // contador
        mov ecx, 0;                        // vector1[i]
        mov edx, 0;                        // vector2[i]
    loop_Sumar:
        mov ecx, [vector1 + esi * 4];
        mov edx, [vector2 + esi * 4];
        add eax, [ecx];                    // multiplican el índice por 4
        add eax, edx;
        inc esi;                           // porque cada elemento ocupa 4 bytes.
        cmp esi, ebx;                      // comparamos con 6 porque es el índice del primer elemento fuera del vector.
        jl loop_Sumar;                     // recorremos la matriz con un solo índice para sumarla
        mov esi, 0;                        // esi será el índice para acceder a los datos
        add resSuma, eax;
    };
};

// Programa principal del benchmark
int main() {

    // Archivo de salida de los resultados.
    FILE* fp;
    fopen_s(&fp, "salida_MMX.txt", "w");

    if (fp == 0) {
        printf("Error creando archivo de salida.");
        return 1;
    }

    // Imprimimos los datos del ordenador.
    char command[200];
#ifdef _WIN32
    // Muestra y guarda el tipo de procesador.
    strcpy_s(command, "wmic cpu get caption, deviceid, name, numberofcores, maxclockspeed, status && wmic cpu get caption, deviceid, name, numberofcores, maxclockspeed, status > HWInfo.txt");
    system(command);

    // Muestra y guarda la información del SO
    strcpy_s(command, "systeminfo && systeminfo > SWInfo.txt");
    system(command);
#endif

#ifdef _unix
    // Muestra y guarda el tipo de procesador
    strcpy_s(command, "lshw -short && lshw -short > HWInfo.txt");
    system(command);

    // Muestra y guarda la información del SO
    strcpy_s(command, "name -a > SWInfo.txt");
    system(command);
#endif

    // Declaracion de los vectores.
    int* vector1;
    int* vector2;

    // Declaracion de los tiempos.
    double duracion = 0, total = 0;

    // Cantidad de iteraciones por tamaño.
    int loop = 3000000;

    printf("Realizando calculos... \n");

    fprintf(fp, "Tamanio\t\tDuracion\n--------------------------- \n");
    printf("Tamanio\t\tDuracion\n--------------------------- \n");


    // Bucle para multiplciar.
    for (int size = 100; size <= 130; size++) {

        vector1 = (int*)malloc(size * sizeof(int));
        vector2 = (int*)malloc(size * sizeof(int));

        if (vector1 == NULL || vector2 == NULL) {
            printf("No se pudo asignar memoria para los vectores.");
            return 1;
        }

        rellenar(vector1, vector2, size);

#ifdef _WIN32
        StartCounter();
#endif
#ifdef _unix
        clock_t begin = clock();
#endif

        MMXsumar(vector1, vector2, size);

#ifdef _unix
        clock_t end = clock();
#endif

        double time;

#ifdef _WIN32
        time = GetCounter();
#endif
#ifdef _unix
        time = (float)((end - begin) / loop / CLOCKS_PER_SEC) * 1000;
#endif

        free(vector1);
        free(vector2);


        fprintf(fp, "%d\t\t", size);
        fprintf(fp, "%f s\n", time);

        printf("%d\t\t", size);
        printf("%f s\n", time);

        total += time;
    };

    fprintf(fp, "==> Media duracion de multiplicacion \t%f s \n\n", (total / (30)));
    printf("==> Media duracion de multiplicacion \t%f s \n\n", (total / (30)));

    // Bucle para sumar.
    duracion = 0;
    total = 0;

    for (int size = 100; size <= 130; size++) {

        vector1 = (int*)malloc(size * sizeof(int));
        vector2 = (int*)malloc(size * sizeof(int));

        if (vector1 == NULL || vector2 == NULL) {
            printf("No se pudo asignar memoria para los vectores.");
            return 1;
        }

        rellenar(vector1, vector2, size);

#ifdef _WIN32
        StartCounter();
#endif
#ifdef _unix
        clock_t begin = clock();
#endif

        MMXmultiplicar(vector1, vector2, size);

#ifdef _unix
        clock_t end = clock();
#endif


        double time;

#ifdef _WIN32
        time = GetCounter();
#endif
#ifdef _unix
        time = (float)((end - begin) / loop / CLOCKS_PER_SEC) * 1000;
#endif

        free(vector1);
        free(vector2);


        fprintf(fp, "%d\t\t", size);
        fprintf(fp, "%f s\n", time);

        printf("%d\t\t", size);
        printf("%f s\n", time);

        total += time;
    };

    fprintf(fp, "==> Duracion media de suma \t%f s \n\n", (total / (30)));
    printf("==> Duracion media de suma \t%f s \n\n", (total / (30)));

    printf("Aprieta INTRO para cerrar.\n");
    const char x = getchar();


    return 0;
};