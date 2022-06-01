#include<iostream>
#include<fstream>
#include<vector>
#include<array>
#include<string>
#include<sstream>
#include"draw.hpp"

static auto const helpMenu =
"╔════════════════════════════════════════════════════════════════════════════════════════════╗\n"
"║                                                                                            ║\n"
"║         ██████╗ ███████╗███╗   ██╗███████╗██████╗  █████╗ ████████╗ ██████╗ ██████╗        ║\n"
"║        ██╔════╝ ██╔════╝████╗  ██║██╔════╝██╔══██╗██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗       ║\n"
"║        ██║  ███╗█████╗  ██╔██╗ ██║█████╗  ██████╔╝███████║   ██║   ██║   ██║██████╔╝       ║\n"
"║        ██║   ██║██╔══╝  ██║╚██╗██║██╔══╝  ██╔══██╗██╔══██║   ██║   ██║   ██║██╔══██╗       ║\n"
"║        ╚██████╔╝███████╗██║ ╚████║███████╗██║  ██║██║  ██║   ██║   ╚██████╔╝██║  ██║       ║\n"
"║         ╚═════╝ ╚══════╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝       ║\n"
"║                                                                                            ║\n"
"╠════════════════════════════════════════════════════════════════════════════════════════════╣\n"
"║                                          Commands                                          ║\n"
"╠════════════════════════════════════════════════════════════════════════════════════════════╣\n"
"║                                                                                            ║\n"
"║                          Generate a Plane: plane [side] [filename]                         ║\n"
"║                                                                                            ║\n"
"║            Generate a Box: box [sideX] [sideY] [sideZ] [nDivs (optional)] [filename]       ║\n"
"║                                                                                            ║\n"
"║               Generate a Sphere: sphere [angleRad] [slices] [stacks] [filename]            ║\n"
"║                                                                                            ║\n"
"║            Generate a Cone: cone [radiusBase] [height] [slices] [stacks] [filename]        ║\n"
"║                                                                                            ║\n"
"║            Generate a Torus: torus [distance] [radius] [slices] [stacks] [filename]        ║\n"
"║                                                                                            ║\n"
"║                    Generate Patch: patch [filenameIN] [tess] [filenameOUT]                 ║\n"
"║                                                                                            ║\n"
"╠════════════════════════════════════════════════════════════════════════════════════════════╣\n"
"║                                          Authors                                           ║\n"
"╠════════════════════════════════════════════════════════════════════════════════════════════╣\n"
"║                 Ariana Lousada                           Carolina Vila Chã                 ║\n"
"║                  Sofia Santos                               Rui Armada                     ║\n"
"╚════════════════════════════════════════════════════════════════════════════════════════════╝\n";

void writeInFile(std::array<std::vector<Point>,3> pontos, std::string filename) {
	std::ofstream file (filename);

	file << pontos[0].size() << std::endl;

	for(Point ponto : pontos[0]){
		file << ponto.x() << ',' << ponto.y() << ',' << ponto.z() << std::endl;
	}
	for(Point normal : pontos[1]){
		file << normal.x() << ',' << normal.y() << ',' << normal.z() << std::endl;
	}
	for(Point textura : pontos[2]){
		file << textura.x() << ',' << textura.y() << std::endl;
	}

	file.close();
}

int main(int argc, char** argv) {
	auto command = std::string(argv[1]);

	if(command == "plane") {
		double lado = std::stod(argv[2]);
		writeInFile(drawPlane(lado), std::string(argv[3]));

	}
	else if (command == "box") {
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
	else if (command == "sphere") {
		double radius = std::stod(argv[2]);
		int slices = std::stoi(argv[3]);
		int stacks = std::stoi(argv[4]);
		writeInFile(drawSphere(radius,slices,stacks), std::string(argv[5]));
	}
	else if (command == "cone"){
		double raioB = std::stod(argv[2]);
		double altura = std::stod(argv[3]);
		int slices = std::stoi(argv[4]);
		int stacks = std::stoi(argv[5]);
		writeInFile(drawCone(raioB,altura,slices,stacks), std::string(argv[6]));
	}
	else if (command == "torus"){
		double distancia = std::stod(argv[2]);
		double raio = std::stod(argv[3]);
		int slices = std::stoi(argv[4]);
		int stacks = std::stoi(argv[5]);
		writeInFile(drawTorus(distancia,raio,slices,stacks), std::string(argv[6]));
	}
	else if(command == "patch"){
		int tess = std::atoi(argv[3]);
		writeInFile(drawPatch(argv[2],tess), std::string(argv[4]));
	}
	else if(command == "help"){
		std::cout << helpMenu << std::endl;
	}
    else {
		std::cout << "-------------->>> ERROR 404 <<<-------------" << std::endl;
		std::cout << "-------->>> Try the help command <<<--------" << std::endl;
	}
}
