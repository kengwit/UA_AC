#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <chrono>
#include <fstream>
#include <string.h>
#include "malloc.h"

// Rellena los arrays con valores ascendentes dependiendo del tama�o del propio array.
void rellenar(int* vector1, int* vector2, int size) {
    for (int index = 0; index < size; index++) {
        vector1[index] = (index);
        vector2[index] = (index);
    };
};

// Multiplica los arrays.
void multiplicar(int* vector1, int* vector2, int size) {
    for (int i = 0; i < size; i++) {
        const int x = (vector1[i] * vector2[i]);
    };
};

// Suma los arrays.
void sumar(int* vector1, int* vector2, int size) {
    for (int i = 0; i < size; i++) {
        const int x = (vector1[i] + vector2[i]);
    };
};

// Programa principal del benchmark
int main() {

    // Archivo de salida de los resultados.
    FILE* fp;
    fopen_s(&fp, "salida_SEE.txt", "w");

    if (fp == 0) {
        printf("Error creando archivo.");
        return 1;
    }

    // Imprimimos los datos del ordenador.
    char command[200];
#ifdef _WIN32
    // Muestra y guarda el tipo de procesador.
    strcpy_s(command, "wmic cpu get caption, deviceid, name, numberofcores, maxclockspeed, status && wmic cpu get caption, deviceid, name, numberofcores, maxclockspeed, status > HWInfo.txt");
    system(command);

    // Muestra y guarda la informaci�n del SO
    strcpy_s(command, "systeminfo && systeminfo > SWInfo.txt");
    system(command);
#endif

#ifdef _unix
    // Muestra y guarda el tipo de procesador
    strcpy_s(command, "lshw -short && lshw -short > HWInfo.txt");
    system(command);

    // Muestra y guarda la informaci�n del SO
    strcpy_s(command, "name -a > SWInfo.txt");
    system(command);
#endif


    // Declaracion de los vectores.
    int* vector1;
    int* vector2;

    // Declaracion de los tiempos.
    float duracion = 0.0, total = 0.0;

    // Cantidad de iteraciones por tama�o.
    int loop = 1000000;

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

        clock_t begin = clock();

        __asm {

            movups xmm0, [vector1] // load vector1 into xmm0
            movups xmm1, [vector2] //load vector2 into xmm1

            mulps xmm0, xmm1 // vector1 * vector2
        };

        clock_t end = clock();

        duracion = (float)(end - begin) / loop;

        free(vector1);
        free(vector2);

        float a = (float)(duracion / CLOCKS_PER_SEC) * 10000;

        fprintf(fp, "%d\t\t", size);
        fprintf(fp, "%f ms\n", a);

        printf("%d\t\t", size);
        printf("%f ms\n", a);

        total += (float)(duracion / CLOCKS_PER_SEC) * 10000;
    };

    fprintf(fp, "==> Media duracion de multiplicacion \t%f ms \n\n", (total / (30)));
    printf("==> Media duracion de multiplicacion \t%f ms \n\n", (total / (30)));

    // Bucle para sumar.
    duracion = 0.0;
    total = 0.0;

    for (int size = 100; size <= 130; size++) {

        vector1 = (int*)malloc(size * sizeof(int));
        vector2 = (int*)malloc(size * sizeof(int));

        if (vector1 == NULL || vector2 == NULL) {
            printf("No se pudo asignar memoria para los vectores.");
            return 1;
        }

        rellenar(vector1, vector2, size);

        clock_t begin = clock();

        __asm {

            movups xmm0, [vector1] // load vector1 into xmm0
            movups xmm1, [vector2] //load vector2 into xmm1

            addps xmm0, xmm1 // vector1 + vector2
        };

        clock_t end = clock();

        duracion = (float)(end - begin) / loop;

        free(vector1);
        free(vector2);
        
        float a = (float)(duracion / CLOCKS_PER_SEC) * 10000;

        fprintf(fp, "%d\t\t", size);
        fprintf(fp, "%f ms\n", a);

        printf("%d\t\t", size);
        printf("%f ms\n", a);

        total += (float)(duracion / (float)CLOCKS_PER_SEC) * 10000;
    };

    fprintf(fp, "==> Duracion media de suma \t%f ms \n\n", (total / (30)));
    printf("==> Duracion media de suma \t%f ms \n\n", (total / (30)));

    printf("Aprieta INTRO para cerrar.\n");
    const char x = getchar();

    return 0;
};