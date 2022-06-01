# GEngine

OpenGL based engine that parses a XML file containing a scene

## Authors
* [Ariana Lousada](https://github.com/AITK42)
* [Carolina Vila Chã](https://github.com/carolinavc99)
* [Rui Armada](https://github.com/RuiArmada)
* [Sofia Santos](https://github.com/RisingFisan)

## How to run

### Generator
Please execute main.cpp with
```
g++ .\faseX\generator\bezier.cpp .\faseX\generator\box.cpp .\faseX\generator\cone.cpp .\faseX\generator\main.cpp .\faseX\generator\plane.cpp .\faseX\generator\sphere.cpp .\faseX\generator\torus.cpp .\faseX\utils\point.cpp -Iinclude -o generator -std=c++11
```

### Engine

Run in this order

`mkdir build`

`cd build`

`cmake .. -DTOOLKITS_FOLDER:STRING=[path to toolkits folder] -A Win32`

`cmake --build .`

`.\Debug\Engine.exe [XML File]`

## Grade

⭐ (15.35/20)
