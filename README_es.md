# Plane: un juego para Atari 2600

### Hardware: limitaciones y características
Resolución de pantalla de solo 160 x 192 pixels.

128 bytes de memoria RAM (solo 128, no 128 Kb ni 128 Mb...).

4 kb de espacio en ROM para el programa.

Sin memoria de video. Se debía dibujar cada pixel, línea por línea, frame a frame, teniendo en cuenta la posición del haz de electrones del Televisor CRT.

Se requieren muy avanzadas habilidades de programación, haciendo uso inteligente de cada ciclo del CPU y en el momento justo.

Solo 2 objetos "jugadores", de 8 pixels de ancho. 2 misiles, de 1 pixel de ancho cada uno, y una "bola" de 1 pixel de ancho. Solo eso y un fondo de (más) baja resolución era lo disponible para hacer un juego completo.


### Cómo probarlo
Clone repository on Windows.
Execute compile.bat, it will regenerate plane.bin file, wich is the ROM of the game. This can be burned in a real eeprom cartridge or used with an emulator.

Execute run.bat to launch the game on Stella emulator.

DASM Assembler and Stella emulator are included in the repository.


### Screenshots showing progress
![This is an image](/screenshots/plane1.gif)
![This is an image](/screenshots/plane2.gif)
![This is an image](/screenshots/plane3a.gif)
![This is an image](/screenshots/plane3b.gif)
![This is an image](/screenshots/plane4.gif)
![This is an image](/screenshots/plane5a.gif)
![This is an image](/screenshots/plane5b.gif)
![This is an image](/screenshots/plane5c.gif)
![This is an image](/screenshots/plane5d.gif)
