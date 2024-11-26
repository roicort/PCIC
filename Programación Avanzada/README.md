# Tarea 4 - Programación en Haskell

Este repositorio contiene la cuarta tarea del curso de Programación Avanzada escrita en Haskell.

## Ejecución

Para ejecutar el programa, compilar y ejecutar el archivo principal:

```bash
ghc T4.hs && ./T4
```

## Salida

El programa llamará a la función `main` que imprimirá en consola el resultado de las funciones implementadas.

```bash
Area de un circulo de radio 5: 78.54
Area de un circulo de radio 10: 314.16

Listas [1,2,3] [3,2,1] iguales: True
Listas [1,2,3] [1,4,3] iguales: False

Ackermann 3 3: 61
Ackermann 3 4: 125

Raices a=1 b=2 c=1: Una raíz real: x = -1.00
Raices a=1 b=-3 c=2: Dos raíces reales: x1 = 2.00, x2 = 1.00
Raices a=1 b=2 c=5: Dos raíces complejas: x1 = -1.00 + 2.00i, x2 = -1.00 - 2.00i

Until (> 100) (* 2) 1: 128
Until (>= 100) (+ 10) 1: 101
Until (== 100) (+ 1) 1: 100
```