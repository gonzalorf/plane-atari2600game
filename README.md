# Plane: an Atari 2600 game

### Hardware limitations and "features"
Screen resolution of 160 x 192 pixels.

128 bytes of RAM (just 128, not 128 Kb nor 128 Mb...).

4 kb of ROM space for the game program.

No video memory. You need to draw every pixel, line by line, frame by frame, according to the TV electron beam position.

Super challenging programming skills are required, making smart use of every CPU cycle in the right moment.

Only 2 sprites, 8 pixels wide. 2 missiles, 1 pixel wide each, and a 1 pixel wide "ball". That, and a low (even lower) resolution background is all you have to make a game.


### How to test it
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
