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
7:5-7:9: 
Empieza el programa.

8:4-8:15: 
Se declaran las variables 'a', 'b' y 'c'.

9:4-9:11: 
Se declara la variable 'T'.

10:8-10:9: 
La expresión ha dado 1.

10:4-10:8: 
Asignamos el valor.

11:8-11:9: 
La expresión ha dado 2.

11:4-11:8: 
Asignamos el valor.

12:8-12:9: 
La expresión ha dado 3.

12:4-12:8: 
Asignamos el valor.

13:9-13:18: 
Se declara la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 0.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 0.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 1.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 1.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 0.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 2.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 1.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 3.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 0.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 4.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 1.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 5.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 0.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 6.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 1.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 7.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 0.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 8.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'true', entramos en el for.

14:14-14:19: 
La expresión ha dado 1.

14:5-14:14: 
Asignamos el valor.

15:16-15:17: 
La expresión ha dado 9.

15:7-15:16: 
Asignamos el valor.

13:28-13:31: 
Se incrementa la variable 'i'.

13:20-13:26: 
La condición vale 'false', salimos del for.

17:17-17:23: 
Se escribe a la salida.
OUTPUT: "0"

17:27-17:36: 
Se escribe a la salida.
OUTPUT: "
"

18:17-18:23: 
Se escribe a la salida.
OUTPUT: "1"

18:27-18:36: 
Se escribe a la salida.
OUTPUT: "
"

19:1-19:2: 
Termina el programa.

