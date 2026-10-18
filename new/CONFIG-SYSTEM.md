- Queremos poder configurar cada cosa.

- Declarar las opciones de configuración:
    - La configuración será jerárquica, se puede usar el tema de los IDs
      del AST?
    - Si una opción como `strings` está activa, implica todas las configuraciones "hijas".
	    - Si está desactivada, todas las hijas desactivadas.
		- Por tanto una configuración que es un conjunto puede tener los estados: "none", "some", y "all".

- Declarar los "niveles" de configuración (estándar):
	- Listados de configuración concretos.
	
- La configuración se tiene que leer (de parámetros o del fichero `.minicc`).
	- También se tienen que poder aplicar argumentos en CLI... `--no-strings`, `--no-*`.

- La configuración se tiene que aplicar, por tanto el código tiene que
  testear cada cosa aparte. Esto complicará un poco todo el código pero
  es cuestión de estandarizar bien.

- Script que genera código como en keywords.gen.sh?
