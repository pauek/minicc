#include <cassert>
#include <iostream>
#include <map>
using namespace std;

#include "translator.hh"

Translator Translator::translator;

const char* Translator::_translations[100][Translator::NUM_LANGS] = { /* 
   { 
      English, 
      Spanish, 
      Catalan 
   } */
   {
      "UNIMPLEMENTED",
      "NO IMPLEMENTADO",
      "NO IMPLEMENTAT"
   }, {
      "Compilation Error", 
      "Error de compilación", 
      "Error de compilació" 
   }, {
      "Execution Error",
      "Error de ejecución",
      "Error d'execució"
   }, {
      "UNIMPLEMENTED",
      "NO IMPLEMENTADO",
      "NO IMPLEMENTAT"
   }, {
      "Unexpected character '%c'",
      "Caracter '%c' inesperado",
      "Caracter '%c' inesperat"
   }, {
      "The '%s' function does not exist.",
      "La función '%s' no existe.",
      "La funció '%s' no existeix."
   }, {
      "Indirect call to functions is not implemented", 
      "La llamada no-directa a funciones no se ha implementado",
      "La crida indirecta a funcions no s'ha implementat"
   }, {
      "An if's condition needs to be a bool value",
      "La condición de un 'if' debe ser un valor de tipo 'bool'",
      "La condició d'un 'if' ha de ser un valor Booleà"
   }, {
      "Expected '}' but end of text found",
      "Esperaba '}' pero he llegado al final del texto",
      "Esperava '}' però he arribat al final del text"
   }, {
      "Expected ';' after expression",
      "Esperaba un ';' después de la expresión",
      "Esperava un ';' després de l'expressió"
   }, {
      "You should specify a filename",
      "No has indicado el fichero de código",
      "No has indicat el fitxer de codi"
   }, {
      "Error when reading input",
      "Error al leer de la entrada",
      "Error al llegir de l'entrada"
   }, {
      "Unexpected '%s' here.",
      "No esperaba '%s' aquí.",
      "No esperava '%s' aquí."
   }, {
      "ignoring macro '%s'",
      "ignorando la macro '%s'",
      "ignorant la macro '%s'"
   }, {
      "'#include' missing closing '%c'",
      "Al '#include' le falta el '%c' de cerrar",
      "A l'#include li falta el '%c' de tancar"
   }, {
      "Expected '%s' here.",
      "Esperaba un '%s' aquí.",
      "Esperava un '%s' aquí."
   }, {
      "Expected an identifier here.",
      "Esperaba un identificador aquí.",
      "Esparaba un identificador aquí."
   }, {
      "Basic types are not templates",
      "Los tipos básicos no son plantillas",
      "Els tipus bàsics no són plantilles"
   }, {
      "Unexpected character '%c' in parameter list",
      "Caracter '%c' inesperado en la lista de parámetros",
      "Caràcter '%c' inesperat a la llista de paràmetres"
   }, {
      "Expected '\"' or '<' here.",
      "Esperaba '\"' o '<' aquí.",
      "Esperava '\"' o '<' aquí."
   }, {
      "Expected an integer here.",
      "Esperaba un entero aquí.",
      "Esperaba un enter aquí."     
   },
      
   { "END" }
};
