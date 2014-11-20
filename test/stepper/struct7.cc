#include <iostream>

struct X {
   int a, b;
};

int main() {
   int a, b, c;
   X T[10];
   a = 1;
   b = 2;
   c = 3;
   for (int i = 0; i < 10; i++) {
   	T[i].a = i % 2;
      T[i].b = i;
   }
   std::cout << T[0].a << std::endl;
   std::cout << T[1].b << std::endl;
}
[[out]]-------------------------------------------------
7:4-7:8: main
Empieza el programa.

8:3-8:15: int a, b, c;
Se declaran las variables 'a', 'b' y 'c'.

9:3-9:11: X T[10];
Se declara la variable 'T'.

10:7-10:8: 1
La expresión ha dado 1.

10:3-10:7: a = 
Asignamos el valor.

11:7-11:8: 2
La expresión ha dado 2.

11:3-11:7: b = 
Asignamos el valor.

12:7-12:8: 3
La expresión ha dado 3.

12:3-12:7: c = 
Asignamos el valor.

13:8-13:18: int i = 0;
Se declara la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 0.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 0.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 1.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 1.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 0.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 2.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 1.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 3.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 0.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 4.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 1.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 5.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 0.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 6.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 1.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 7.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 0.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 8.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'true', entramos en el for.

14:13-14:18: i % 2
La expresión ha dado 1.

14:4-14:13: T[i].a = 
Asignamos el valor.

15:15-15:16: i
La expresión ha dado 9.

15:6-15:15: T[i].b = 
Asignamos el valor.

13:27-13:30: i++
Se incrementa la variable 'i'.

13:19-13:25: i < 10
La condición vale 'false', salimos del for.

17:16-17:22: T[0].a
Se escribe a la salida.
OUTPUT: "0"

17:26-17:35: std::endl
Se escribe a la salida.
OUTPUT: "
"

18:16-18:22: T[1].b
Se escribe a la salida.
OUTPUT: "1"

18:26-18:35: std::endl
Se escribe a la salida.
OUTPUT: "
"

19:0-19:1: }
Termina el programa.

