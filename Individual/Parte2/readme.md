# MMX
MMX es un conjunto de instrucciones SIMD (Single Instruction Multiple Data) creadas por Intel.
La tecnología MMX fue diseñada para mejorar de forma sustancial el rendimiento de las aplicaciones multimedia y de telecomunicaciones.
Se basa en un nuevo juego de instrucciones, que se añadían a las ya existentes en la arquitectura 80×86, y en nuevos tipos de datos de 64 bits.
Dichas instrucciones trabajaban en paralelo sobre múltiples elementos de datos empaquetados en cantidades de 64 bits.

https://docs.oracle.com/cd/E18752_01/html/817-5477/eojdc.html


# SSE (Streaming SIMD Extensions)
SSE son una extensión al grupo de instrucciones MMX, que también fueron desarrolladas por Intel, concretamente, para sus procesadores Pentium III (1999).
Están especialmente pensadas para decodificación de MPEG2 (códec vinculado a los DVD), procesamiento de gráficos tridimensionales y software de reconocimiento de voz.
Con la tecnología SSE, los microprocesadores x86 fueron dotados de setenta nuevas instrucciones y de ocho registros nuevos: del xmm0 al xmm7.
Estos registros tienen una extensión de 128 bits (es decir que pueden almacenar hasta 16 bytes de información cada uno).

https://docs.oracle.com/cd/E26502_01/html/E28388/eojde.html


## NET Bubble Sort SEE (comparando assembler + C)
- ¿Qué hace el programa?


- ¿Se utilizan extensiones MMX o SSE?


- Comenta el funcionamiento de la función: void sortDoublesSSE(Int32 byteCount, double* values)


- ¿Qué ganancia obtenemos con el algoritmo MMX/SSE con respecto al algoritmo secuencial? Realiza
una batería de pruebas y muéstralo utilizando gráficas explicativas.


## NET Data Transfer (comparando assembler + C)
- ¿Qué hace el programa?


- Explica las siguientes instrucciones: shufpd, cmpltpd, movmskpd


- Explica el funcionamiento de la siguiente función: int DataTransferOptimised(int* piDst, int* piSrc, unsigned long SizeInBytes)


- ¿Qué ganancia obtenemos con el algoritmo optimizado mediante extensiones SIMD con respecto al algoritmo secuencial? Realiza una batería de pruebas y muéstralo utilizando gráficas explicativas.


## NET Inner Product (comparando assebler + C)
- ¿Qué hace el programa?


- Comenta la siguiente función: float sse3_inner(const float* a, const float* b, unsigned int size)


- ¿Qué ganancia obtenemos con el algoritmo optimizado mediante extensiones SIMD con respecto al algoritmo secuencial? Realiza una batería de pruebas y muéstralo utilizando gráficas explicativas.


## NET MatrixVectorMult (comparando assembler + C)
- ¿Que hace el programa?


- ¿Qué ganancia obtenemos con el algoritmo optimizado mediante extensiones SIMD con respecto al algoritmo secuencial?


- Realiza una batería de pruebas y muéstralo utilizando gráficas explicativas.


