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
7:5-7:9: main
Empieza el programa.

8:4-8:15: int a, b, c
Se declaran las variables 'a', 'b' y 'c'.

9:4-9:11: X T[10]
Se declara la variable 'T'.

10:8-10:9: 1
La expresión ha dado 1.

10:4-10:8: a = 
Asignamos el valor.

11:8-11:9: 2
La expresión ha dado 2.

11:4-11:8: b = 
Asignamos el valor.

12:8-12:9: 3
La expresión ha dado 3.

12:4-12:8: c = 
Asignamos el valor.

13:9-13:18: int i = 0
Se declara la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 0.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 0.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 1.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 1.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 0.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 2.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 1.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 3.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 0.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 4.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 1.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 5.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 0.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 6.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 1.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 7.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 0.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 8.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'true', entramos en el for.

14:14-14:19: i % 2
La expresión ha dado 1.

14:5-14:14: T[i].a = 
Asignamos el valor.

15:16-15:17: i
La expresión ha dado 9.

15:7-15:16: T[i].b = 
Asignamos el valor.

13:28-13:31: i++
Se incrementa la variable 'i'.

13:20-13:26: i < 10
La condición vale 'false', salimos del for.

17:17-17:23: T[0].a
Se escribe a la salida.
OUTPUT: "0"

17:27-17:36: std::endl
Se escribe a la salida.
OUTPUT: "
"

18:17-18:23: T[1].b
Se escribe a la salida.
OUTPUT: "1"

18:27-18:36: std::endl
Se escribe a la salida.
OUTPUT: "
"

19:1-19:2: }
Termina el programa.

