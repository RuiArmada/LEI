# Projeto-CG

OpenGL based engine that parses a XML file containing a scene

![trand_leite](images/pp840x830-pad1000x1000f8f8f8.jpg)

## Authors
* [Rui Filipe Pimenta Armada](https://github.com/RuiArmada)
* [Carolina da Torre Vila Chã](https://github.com/carolinavc99)
* [Sofia Guilherme Rodrigues dos Santos](https://github.com/RisingFisan)
* [Ariana Raquel Barreira Lousada](https://github.com/AITK42)


## Generator
Please execute main.cpp with
```
g++ .\fase3\generator\bezier.cpp .\fase3\generator\box.cpp .\fase3\generator\cone.cpp .\fase3\generator\main.cpp .\fase3\generator\plane.cpp .\fase3\generator\sphere.cpp .\fase3\generator\torus.cpp .\fase3\utils\point.cpp -Iinclude -o generator -std=c++11
```

## Engine

Run in this order

`mkdir build`

`cd build`

`cmake .. -DTOOLKITS_FOLDER:STRING=[path para a pasta dos toolkits] -A Win32`

`cmake --build .`

`.\Debug\Engine.exe [ficheiro XML]`
