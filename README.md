# Tarea 1: Motor de Aventura de Texto (CI-3661)

- **Nombres:** Kevin Briceño
- **Carnets:** 15-11661

---

## Cómo Compilar y Ejecutar

Este proyecto usa `stack`. Para compilarlo y ejecutarlo:

1.  **Compilar:** `stack build`
2.  **Ejecutar:** `stack exec TextAdventureEngine-exe`

---
## Justificación de Diseño


### 1. Elección de Estructuras de Datos

Usamos Map NombreSala Sala (habitaciones) y Map NombreObjeto Objeto (objetos) en lugar de listas. Justificación:

Ventaja: búsquedas por nombre, inserciones y actualizaciones claras y eficientes (O(log n)). Muy útil cuando el mundo crece.

Desventaja: código ligeramente más verboso que con listas, pero más seguro y escalable.

### 2. Separación de Lógica Pura e Impura

Engine.Core contiene funciones puras (procesarComando) que no hacen IO; devuelven (String, EstadoJuego).

Engine.Persistence y Main hacen la I/O (leer archivo, leer teclado, escribir en consola).

Beneficio: puedes testear procesarComando con HUnit/tasty sin tocar archivos o consola. Esto es la aplicación práctica de las clases sobre pureza y mónadas.

#### Tipado y ADTs

Direccion, Objeto, Sala, EstadoJuego son ADTs. Esto fuerza comprobación en tiempo de compilación y reduce errores de strings mágicos.

#### Parser simple

parsearComando es deliberadamente simple y tolerante. Evita parsers complejos para la entrega inicial; si quieres se puede reemplazar por combinator parsing después.

#### Validaciones

cargarMundo valida referencias cruzadas (objetos referenciados existen, salidas apuntan a salas existentes). Esto evita errores en tiempo de ejecución por datos mal formados.