// BenchmarkC.cpp : Este archivo contiene la función "main". La ejecución del programa comienza y termina ahí.

#include <iostream>
#include <chrono>
#include <fstream>
using namespace std;
using namespace chrono;

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

void rellenar(int* vector1, int* vector2, int size) {
    for (int index = 0; index < size; index++) {
        vector1[index] = (index);
        vector2[index] = (index);
    }
}

void multiplicar(int* vector1, int* vector2, int size) {
    for (int i = 0; i < size; i++) {
        (vector1[i] * vector2[i]);
    }
}

void sumar(int* vector1, int* vector2, int size) {
    for (int i = 0; i < size; i++) {
        (vector1[i] + vector2[i]);
    }
}

void logo() {
    system("systeminfo");
    cout << endl << endl
        << "                      _________________________________________" << endl
        << "                      |            __   ___    ___   __      __ ___     ____    _____  ___       |" << endl
        << "                      |          /  __|  |  _   \\  |    _| |   \\     | | |__   /   |  ___  |  |__   __| |  __  \\       |" << endl
        << "                      |         /  /_     |  | |   | |   |      | |\\ \\    | |      /  /    | |       | |       |  |       |  |   |  \\      |" << endl
        << "                      |        /  __|    |  ||   / |   |__  | | \\ \\   | |   /\\/  /     | |       | |       |  |       |  |   |  |      |" << endl
        << "                      |       /  /          |      /   |   __|  | |  \\ \\  | |   \\/  /      | |       | |       |  |       |  |   |  |      |" << endl
        << "                      |      /  /           |  |\\  \\   |  |       | |   \\ \\ | |   /  /\\      | |       | |       |  |       |  |   |  |      |" << endl
        << "                      |     /  /            |  | \\  \\  |  |__   | |    \\ \\| |  /  /\\/_   | |__| |   __|  |__  |  |_|  /      |" << endl
        << "                      |    /_/             ||  \\\\ |__|  ||     \\_| /__|   |___|  |____| |____/       |"<<endl;
    cout << "                      |________________________________________|" << endl << endl << endl;
    cout << endl << endl;

}

int main() {
    ofstream fs("salida.txt");
    logo();
    int* vector1;
    int* vector2;
    float duracion=0.0, total = 0.0;
    int loop=1000000;
    fs << "Size   Duracion"<<endl;
    for (int size = 100; size <= 130; size++) {
        vector1 = new int[size];
        vector2 = new int[size];
        rellenar(vector1, vector2, size); 

        clock_t begin = clock();
        for (int i = 0; i < loop; i++) {
            multiplicar(vector1, vector2, size);
        }
        clock_t end = clock();

        duracion = (float)(end - begin)/loop;
        delete[] vector1;
        delete[] vector2;
        fs << size<<"    ";
        fs << (float)(duracion / CLOCKS_PER_SEC)*1000000<< char(230) <<"s "<<endl;
        total += (float)(duracion / CLOCKS_PER_SEC) * 1000000;
    }
    cout << endl << "==Media duracion de multiplicacion " << (total / (10030 - 10000)) << char(230) << "s " << endl;
    fs << endl << "==Media duracion de multiplicacion " << (total / (10030 - 10000)) << char(230) << "s " << endl;

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
        fs << size << "    ";
        fs << (float)(duracion / CLOCKS_PER_SEC) * 1000000 << char(230) << "s " << endl;
        total += (float)(duracion / CLOCKS_PER_SEC) * 1000000;
    }

    cout << endl<<"==Media duracion de suma "<<(total/(10030-10000))<< char(230) << "s "<<endl;
    fs << endl << "==Media duracion de suma " << (total / (10030 - 10000)) << char(230) << "s " << endl;

    return 0;
}