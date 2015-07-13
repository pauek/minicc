struct Tuple {
   int a, b, c;
};

int main() {
   const Tuple t = {1, 2};
}
[[err]]--------------------------------------------------
semantic/const/const5.cc[6:15-6:25]: En una tupla constante hay que inicializar todas las casillas.
