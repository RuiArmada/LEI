#include<iostream>
#include<fstream>
#include<vector>
#include<string>
#include<sstream>
#include"draw.hpp"

static auto const helpMenu =
"╔════════════════════════════════════════════════════════════════════════════════════════════╗\n"
"║                                                                                            ║\n"
"║                                 How to use the Generator:                                  ║\n"
"║                                                                                            ║\n"
"╚════════════════════════════════════════════════════════════════════════════════════════════╝\n"
"╔════════════════════════════════════════════════════════════════════════════════════════════╗\n"
"║                         Generate a Plane: plane [side] [filename]                          ║\n"
"║════════════════════════════════════════════════════════════════════════════════════════════║\n"
"║                    Generate a Box: box [sideX] [sideY] [sideZ] [filename]                  ║\n"
"║════════════════════════════════════════════════════════════════════════════════════════════║\n"
"║               Generate a Sphere: sphere [angleRad] [slices] [stacks] [filename]            ║\n"
"║════════════════════════════════════════════════════════════════════════════════════════════║\n"
"║            Generate a Cone: cone [radiusBase] [height] [slices] [stacks] [filename]        ║\n"
"║════════════════════════════════════════════════════════════════════════════════════════════║\n"
"║            Generate a Torus: torus [distance] [radius] [slices] [stacks] [filename]        ║\n"
"║════════════════════════════════════════════════════════════════════════════════════════════║\n"
"║                    Generate Patch: patch [filenameIN] [tess] [filenameOUT] 				  ║\n"
"╚════════════════════════════════════════════════════════════════════════════════════════════╝\n"
"╔════════════════════════════════════════════════════════════════════════════════════════════╗\n"
"║                                                                                            ║\n"
"║                                                                                            ║\n"
"╚════════════════════════════════════════════════════════════════════════════════════════════╝\n";

//add write in file function
//template<typename F>
void writeInFile(std::vector<Point> pontos, std::string filename) {
	std::ofstream file (filename);

	file << pontos.size() << std::endl;

	for(Point ponto : pontos){
		file << ponto.x() << ',' << ponto.y() << ',' << ponto.z() << std::endl;
	}

	file.close();
}

int main(int argc, char** argv) {
	auto figura = std::string(argv[1]);

	if(figura == "plane") {
		double lado = std::stod(argv[2]);
		writeInFile(drawPlane(lado), std::string(argv[3]));

	}
	else if (figura == "box") {
		double comprimento = std::stod(argv[2]);
		double largura = std::stod(argv[3]);
		double altura = std::stod(argv[4]);
		if (argc == 6){
			writeInFile(drawBox(comprimento,largura,altura), std::string(argv[5]));
		}
		else {
			double divisoes = std::stod(argv[5]);
			writeInFile(drawBoxD(comprimento,largura,altura,divisoes), std::string(argv[6]));
		}

	}
	else if (figura == "sphere") {
		double radius = std::stod(argv[2]);
		int slices = std::stoi(argv[3]);
		int stacks = std::stoi(argv[4]);
		writeInFile(drawSphere(radius,slices,stacks), std::string(argv[5]));
	}
	else if (figura == "cone"){
		double raioB = std::stod(argv[2]);
		double altura = std::stod(argv[3]);
		int slices = std::stoi(argv[4]);
		int stacks = std::stoi(argv[5]);
		writeInFile(drawCone(raioB,altura,slices,stacks), std::string(argv[6]));
	}
	else if (figura == "torus"){
		double distancia = std::stod(argv[2]);
		double raio = std::stod(argv[3]);
		int slices = std::stoi(argv[4]);
		int stacks = std::stoi(argv[5]);
		writeInFile(drawTorus(distancia,raio,slices,stacks), std::string(argv[6]));
	}
	else if(figura == "patch"){
		int tess = std::atoi(argv[3]);
		writeInFile(patch(argv[2],tess), std::string(argv[4]));
	}
    else
		std::cout << "ERROR 404." << std::endl;
		std::cout << helpMenu << std::endl;

}
