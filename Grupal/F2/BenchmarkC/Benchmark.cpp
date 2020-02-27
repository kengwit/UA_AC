// BenchmarkC.cpp : Este archivo contiene la función "main". La ejecución del programa comienza y termina ahí.

#include <iostream>
#include <chrono>
#include <fstream>
using namespace std;
using namespace chrono;

// Funcion que imprime los valores de los arrays, solo tiene uso visual / para depurar.
void imprimir(const int* vector, int size) {
    for (int index = 0; index < size; index++) {
        if (index == 0 && index != (size - 1))
            cout << "[" << vector[index] << ", ";
        else if (index == 0 && index == (size - 1))
            cout << "[" << vector[index] << "]" << endl;
        else if (index == (size - 1))
            cout << vector[index] << "]" << endl;
        else
            cout << vector[index] << ", ";
    }
}

// Rellena los arrays con valores ascendentes dependiendo del tamaño del propio array.
void rellenar(int* vector1, int* vector2, int size) {
    for (int index = 0; index < size; index++) {
        vector1[index] = (index);
        vector2[index] = (index);
    }
}

// Multiplica los arrays.
void multiplicar(int* vector1, int* vector2, int size) {
    for (int i = 0; i < size; i++) {
        const int x = (vector1[i] * vector2[i]);
    }
}

// Suma los arrays.
void sumar(int* vector1, int* vector2, int size) {
    for (int i = 0; i < size; i++) {
        const int x = (vector1[i] + vector2[i]);
    }
}

// Programa principal del benchmark
int main() {

    // Archivo de salida de los resultados.
    ofstream fs("salida_C.txt");

    // Imprimimos los datos del ordenador.
    #ifdef _WIN32
        system("systeminfo");
    #endif

    #ifdef _unix
        system("lshw -short");
    #endif

    // Declaracion de los vectores.
    int *vector1;
    int *vector2;

    // Declaracion de los tiempos.
    float duracion = 0.0, total = 0.0;

    // Cantidad de iteraciones por tamaño.
    int loop = 1000000;


    cout << "Realizando calculos..." << endl << endl;

    fs   << "Tamanio\t\tDuracion" << endl << "---------------------------" << endl;

    cout << "Tamanio\t\tDuracion" << endl << "---------------------------" << endl;

    // Bucle para multiplciar.
    for (int size = 100; size <= 130; size++) {
        vector1 = (int *)malloc(size * sizeof(int));
        vector2 = (int *)malloc(size * sizeof(int));

        rellenar(vector1, vector2, size); 

        clock_t begin = clock();

        for (int i = 0; i < loop; i++) {
            multiplicar(vector1, vector2, size);
        }

        clock_t end = clock();

        duracion = (float)(end - begin)/loop;

        delete[] vector1;
        delete[] vector2;

        fs << size << "\t\t";
        fs << (float)(duracion / CLOCKS_PER_SEC) * 10000 << "\tms "<<endl;

        cout << size << "\t\t";
        cout << (float)(duracion / CLOCKS_PER_SEC) * 10000 << "\tms " << endl;

        total += (float)(duracion / CLOCKS_PER_SEC) * 10000;
    }

    cout << "==> Media duracion de multiplicacion \t" << (total / (30))  << "ms " << endl << endl;
    fs   << "==> Media duracion de multiplicacion \t" << (total / (30))  << "ms " << endl << endl;

    // Bucle para sumar.
     duracion = 0.0;
     total = 0.0;

    for (int size = 100; size <= 130; size++) {

        vector1 = new int[size];
        vector2 = new int[size];

        rellenar(vector1, vector2, size);

        clock_t begin = clock();

        for (int i = 0; i < loop; i++) {
            sumar(vector1, vector2, size);
        }

        clock_t end = clock();

        duracion = (float)(end - begin) / loop;

        delete[] vector1;
        delete[] vector2;

        fs << size << "\t\t";
        fs << (float)(duracion / CLOCKS_PER_SEC) * 10000 << "\tms " << endl;

        cout << size << "\t\t";
        cout << (float)(duracion / CLOCKS_PER_SEC) * 10000 << "\tms " << endl;

        total += (float)(duracion / CLOCKS_PER_SEC) * 10000;
    }

    cout << "==> Media duracion de suma \t\t" << (total / (30)) << "ms " << endl;
    fs   << "==> Media duracion de suma \t\t" << (total / (30)) << "ms " << endl;

    return 0;
}