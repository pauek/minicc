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
   cout << T[0].a << endl;
   cout << T[1].b << endl;
}
[[out]]--------------------------------------------------
Saltamos a la función 'main'.
6:3-6:15: int a, b, c;
Se declaran las variables 'a', 'b' y 'c'.
7:3-7:11: X T[10];
Se declara la variable 'T'.
8:7-8:8: 1
La expresión ha dado 1.
8:3-8:7: a = 
Asignamos el valor.
9:7-9:8: 2
La expresión ha dado 2.
9:3-9:7: b = 
Asignamos el valor.
10:7-10:8: 3
La expresión ha dado 3.
10:3-10:7: c = 
Asignamos el valor.
11:8-11:18: int i = 0;
Se declara la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 0.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 0.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 1.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 1.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 0.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 2.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 1.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 3.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 0.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 4.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 1.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 5.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 0.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 6.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 1.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 7.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 0.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 8.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'true', entramos en el for.
12:13-12:18: i % 2
La expresión ha dado 1.
12:4-12:13: T[i].a = 
Asignamos el valor.
13:15-13:16: i
La expresión ha dado 9.
13:6-13:15: T[i].b = 
Asignamos el valor.
11:27-11:30: i++
Se incrementa la variable 'i'.
11:19-11:25: i < 10
La condición vale 'false', salimos del for.
15:11-15:17: T[0].a
Se escribe a la salida.
15:21-15:25: endl
Se escribe a la salida.
16:11-16:17: T[1].b
Se escribe a la salida.
16:21-16:25: endl
Se escribe a la salida.
[[err]]--------------------------------------------------
