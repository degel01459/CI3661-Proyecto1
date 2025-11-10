# Tarea 1: Motor de Aventura de Texto (CI-3661)

- **Nombres:** Kevin Briceño
- **Carnets:** 15-11661

## Estructura del Proyecto
CI3661-PROYECTO1
├─ app
│  └─ Main.hs				          -- ciclo principal y manejo de I/O.
├─ src
│  └─ Engine
│     ├─ Core.hs         	    -- lógica pura del juego (procesamiento de comandos)
│     ├─ Parser.hs       	    -- parseo de comandos y direcciones
│     ├─ Persistence.hs  	    -- carga/validación del mundo desde mundo.txt
│     └─ Types.hs        	    -- ADTs y tipos compartidos
├─ mundo.txt				          -- definición del mundo del juego
├─ package.yaml				        -- configuración del proyecto stack
├─ stack.yaml				          -- configuración de stack
├─ README.md				          -- este archivo
├─ TextAdventureEngine.cabal	-- archivo de configuración cabal
└─ Proyecto_1_Laboratorio_de_Lenguajes_de_programación.pdf

## Cómo Compilar y Ejecutar

Este proyecto usa `stack`. Para compilarlo y ejecutarlo:

1.  **Compilar:** `stack build`
2.  **Ejecutar:** `stack exec TextAdventureEngine-exe`

## Descripción General (detallada)

Este proyecto es un **motor de aventura de texto**:

- **Movimiento entre salas**  
  - Controlado por el Map Direccion Salida. Se utiliza el comando ir <dirección> tras verificar la existencia y el estado de bloqueo de la salida.

- **Exploración / inspección**  
  - Muestra al jugador la descripción de la sala actual, el listado de salidas accesibles, los objetos presentes y la ubicación de los NPCs.

- **Objetos: tomar / usar**  
  - Los comandos tomar <objeto> y usar <objeto> gestionan el inventario y las interacciones. El inventario respeta cantidades y se considera la capacidad de carga como base para futuras restricciones.
  - Comportamiento de usar:
    - Informativos: Muestran el objDesc (ej. nota).
    - Activadores: Setan una bandera (Flag) global (linterna, repelente).
    - Llaves: tryUsarLlaveEnSala intenta desbloquear la primera salida que requiera dicha llave.
    - Consumibles: Restablecen condiciones o salud (vendas, antidoto).
  - El inventario respeta cantidades y hay cálculo de `pesoActual` frente a `capacidad` (base para futuras restricciones).

- **NPCs**  
  - Los NPCs se inicializan a partir de mundo.txt y se reasignan aleatoriamente a salas para aumentar la variabilidad.
  - Interacción: El comando hablar <NPC> realiza una búsqueda insensible a mayúsculas/minúsculas, incrementa la métrica interna de confianza del NPC y devuelve una clasificación simple de su actitud ("Parece aliado / neutral / hostil").

- **Trampas**  
  - Mecánica de Trampas: Al entrar a una sala, la función resolverTrampasAlEntrar evalúa las Trampas de la sala.
    - Si el jugador posee el trapRequiereItem (en inventario o como Flag activo), se evita el daño.
    - De lo contrario, se aplica el trapDaño a saludJugador. Si la salud $\leq 0$, el juego termina (ejecutando = False).
  - Efectos Globales: El estado del jugador se gestiona mediante:
    - flags :: Map String Bool: para estados binarios persistentes (ejemplo: linterna encendida).
    - estadoCondicion: para estados como envenenamiento.

- **Condición de victoria / salida del laberinto**  
  - La Salida del Laberinto es una sala especial que valida requisitos específicos para el término exitoso de la partida:
    - Posesión de la llave_salida.
    - Posesión de 8 moneda_dorada.
  - Si entras en la salida cumpliendo requisitos, el motor termina la partida con un mensaje de laberinto superado. Si no, el jugador recibe un mensaje que explica qué falta.

## Justificación de diseño

### 1) Elección de estructuras de datos

- Aislamiento de la IO: Se aplica la separación. Engine.Core contiene funciones puras como procesarComando :: Comando -> EstadoJuego -> (String, EstadoJuego). Esta función no utiliza la IO, lo que facilita la verificación formal y las pruebas unitarias.

- Responsabilidad de IO: Los módulos Engine.Persistence y app/Main.hs son los únicos responsables de los efectos secundarios (lectura/escritura de archivos, entrada/salida de la consola).

### 2) Separación de lógica pura e impura

- **`Engine.Core`** contiene funciones puras: `procesarComando :: Comando -> EstadoJuego -> (String, EstadoJuego)`.  
- **`Engine.Persistence`** y **`app/Main.hs`** manejan IO: lectura del `mundo.txt`, asignación inicial, interacción con el usuario.

### 3) Tipado fuerte y ADTs

- La definición de Tipos de Datos Algebraicos (Direccion, Salida, Trampa, Comando, etc.) en Types.hs es indispensable.

### 4) Parser deliberadamente simple

- Se utilizó Map NombreSala Sala y Map NombreObjeto Objeto en lugar de listas de tuplas.
- Garantiza acceso directo y eficiente a salas y objetos por nombre, simplificando la lógica de búsqueda y manipulación.

### 5) Validaciones al cargar mundo

- El módulo Persistence.hs incluye lógica de validación cruzada durante cargarMundo.
- Se verifica que todas las referencias (objetos que aparecen en salas, salas destino de las salidas, ítems requeridos por trampas) existan en su respectivo catálogo. Este control previene fallos de runtime causados por un mundo.txt malformado.

## Diseño orientado a evolución

- El uso de banderas globales (flags) y la definición de objetos y trampas en mundo.txt permiten la fácil adición de nuevos objetos, efectos, y reglas de juego sin modificar la lógica central (Core.hs).

## Resumen de lo implementado que afecta la jugabilidad ahora

- Inicio del jugador filtrado para comenzar sólo en salas cuyo nombre empiece por `"Habitacion "` evita comenzar en zonas no diseñadas como la salida.  
- NPCs reasignados aleatoriamente (crea sensación de que se mueven por el laberinto).  
- La idea es que las Trampas se resuelven **al entrar** se muestran mensajes de daño/evitación y pueden matar al jugador si la salud llega a 0.  
- `usar`:
  - lee notas.
  - activa equipos.
  - usa llaves para desbloquear salidas locales.
- Salida final: requiere `llave_salida` + 8 `moneda_dorada`.
