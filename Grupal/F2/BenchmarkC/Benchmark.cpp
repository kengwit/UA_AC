// BenchmarkC.cpp : Este archivo contiene la función "main". La ejecución del programa comienza y termina ahí.

#include <iostream>
#include <chrono>


using namespace std;
using namespace chrono;

void imprimir(const long int* vector, int size) {
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

void rellenar(int* vector1, int* vector2, int size) {
    for (int index = 0; index < size; index++) {
        vector1[index] = (index);
        vector2[index] = (index);
    }
}

void calcula(int* vector1, int* vector2, int size) {
        for (int i = 0; i < size; i++) {
            (vector1[i] * vector2[i]);
            (vector1[i] + vector2[i]);
            (vector1[i] - vector2[i]);
            (vector1[i] / vector2[i]);
            (vector1[i] % vector2[i]);
        }
}

int main() {
    int* vector1;
    int* vector2;
    int loopsPerSize = 10000000;

    for (int size = 1; size <= 30; size++) {
        clock_t begin;
        clock_t end;

        vector1 = new int[size];
        vector2 = new int[size];

        rellenar(vector1, vector2, size);

        begin = clock();
 
        for (int i = 0; i < loopsPerSize; i++) {
            calcula(vector1, vector2, size);
        }

        end = clock();

        delete[] vector1;
        delete[] vector2;

        cout << (float)(end - begin) / CLOCKS_PER_SEC << " s \t ";
    }
    
    return 0;
}