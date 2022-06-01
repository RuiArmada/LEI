#include "parser.hpp"
#include "algebra.hpp"

#define _USE_MATH_DEFINES
#include <math.h>

#include <stdio.h>
#include <fstream>

using namespace tinyxml2;

array<vector<Point>,3> vectorize(const char *filename) {

    std::ifstream file(filename);
    if(!file.good()) {
        file.open(std::string("../").append(filename).c_str());
        if(!file.good()) {
            printf("Error opening file %s\n", filename);
            exit(1);
        }
    }

    std::string line;
    vector<Point> coords;
    vector<Point> normals;
    vector<Point> textures;

    //printf("Reading %s\n", filename);
    std::getline(file,line);

    unsigned long N = std::stoul(line);
    coords.reserve(N);
    for (unsigned long i = 0; i < N; i++) {
        std::getline(file, line);
        double a, b, c;
        int matches = sscanf(line.c_str(), "%lf,%lf,%lf", &a, &b, &c);
        if (matches != 3) {
            printf("ERROR - invalid number of points in vertex %s in line %d of file %s\n", line.c_str(), i + 1, filename);
            exit(1);
        }
        coords.push_back(Point(a, b, c));
    }
    char x = file.get();
    if(!file.eof()) {
        file.putback(x);   
        normals.reserve(N);
        for (unsigned long i = 0; i < N; i++) {
            std::getline(file, line);
            double a, b, c;
            int matches = sscanf(line.c_str(), "%lf,%lf,%lf", &a, &b, &c);
            if (matches != 3) {
                printf("ERROR - invalid number of points in vertex %s in line %d of file %s\n", line.c_str(), N + i + 1, filename);
                exit(1);
            }
            normals.push_back(Point(a, b, c));
        }
        x = file.get();
        if(!file.eof()) {
            file.putback(x);   
            textures.reserve(N);
            for (unsigned long i = 0; i < N; i++) {
                std::getline(file, line);
                double a, b;
                int matches = sscanf(line.c_str(), "%lf,%lf", &a, &b);
                if (matches != 2) {
                    printf("ERROR - invalid number of points in vertex %s in line %d of file %s\n", line.c_str(), N * 2 + i + 1, filename);
                    exit(1);
                }
                textures.push_back(Point(a, b, 0));
            }
        }
    }

    file.close();
    //printf("Finished reading %s\n", filename);
    return {coords, normals, textures};
}

void parseGroup(vector<struct group> & scene, tinyxml2::XMLElement* group, double * matrix, struct group curGroup) {
    bool hasTrans = false, hasRotate = false, hasScale = false, hasModels = false;
    vector<std::array<vector<Point>,3>> solids;
    vector<solidAttribs> attributes;
    double time = -1;

    XMLElement* elem = group->FirstChildElement();
    while(elem) {
        if(!strcmp(elem->Name(), "models")) {
            if(!hasModels) hasModels = true;
            else {
                puts("Error - too many models in the same group.");
                exit(1);
            }
            XMLElement* model = elem->FirstChildElement();
            while(model) {
                solidAttribs attrbs;
                solids.push_back(vectorize(model->Attribute("file")));

                float diffR = 0, diffG = 0, diffB = 0;
                if (model->QueryFloatAttribute("diffR",&diffR) == XML_SUCCESS && model->QueryFloatAttribute("diffG",&diffG) == XML_SUCCESS && model->QueryFloatAttribute("diffB",&diffB) == XML_SUCCESS) {
                    attrbs.diffuse = {diffR/255, diffG/255, diffB/255, 1};
                    attrbs.hasDiffuse = true;
                }

                float specR = 0, specG = 0, specB = 0;
                if (model->QueryFloatAttribute("specR",&specR) == XML_SUCCESS && model->QueryFloatAttribute("specG",&specG) == XML_SUCCESS && model->QueryFloatAttribute("specB",&specB) == XML_SUCCESS) {
                    attrbs.specular = {specR/255, specG/255, specB/255, 1};
                    attrbs.hasSpecular = true;
                }

                float emisR = 0, emisG = 0, emisB = 0;
                if (model->QueryFloatAttribute("emisR",&emisR) == XML_SUCCESS && model->QueryFloatAttribute("emisG",&emisG) == XML_SUCCESS && model->QueryFloatAttribute("emisB",&emisB) == XML_SUCCESS) {
                    attrbs.emissive = {emisR/255, emisG/255, emisB/255, 1};
                    attrbs.hasEmissive = true;
                }

                float ambR = 0, ambG = 0, ambB = 0;
                if (model->QueryFloatAttribute("ambR",&ambR) == XML_SUCCESS && model->QueryFloatAttribute("ambG",&ambG) == XML_SUCCESS && model->QueryFloatAttribute("ambB",&ambB) == XML_SUCCESS) {
                    attrbs.ambient = {ambR/255, ambG/255, ambB/255, 1};
                    attrbs.hasAmbient = true;
                }

                const char * textureFile = model->Attribute("texture");
                if (textureFile) {
                    attrbs.textureFile = std::string(textureFile);
                }

                attributes.push_back(attrbs);
                model = model->NextSiblingElement();
            }
        }
        else if(!strcmp(elem->Name(), "translate")) {
            if(!hasTrans) hasTrans = true;
            else {
                puts("Error - too many transformations applied to the same group.");
                exit(1);
            }
            if (elem->QueryDoubleAttribute("time", &time) == XML_SUCCESS) {
                transformation tf;
                memcpy(&tf.matrix, matrix, 16 * sizeof(double));
                tf.time = -1;
                curGroup.transformations.push_back(tf);
                double idM[16] = {1,0,0,0,
                                0,1,0,0,
                                0,0,1,0,
                                0,0,0,1};
                matrix = idM;

                transformation new_tf;
                new_tf.time = time;

                XMLElement* child_elem = elem->FirstChildElement();
                while (child_elem) {
                    if(strcmp(child_elem->Name(),"point")) {
                        printf("Error - invalid element in translation - expected \"point\", got \"%s\"\n",child_elem->Name());
                        exit(1);
                    }
                    double x = child_elem->DoubleAttribute("X");
                    double y = child_elem->DoubleAttribute("Y");
                    double z = child_elem->DoubleAttribute("Z");
                    new_tf.points.push_back({x,y,z});
                    child_elem = child_elem->NextSiblingElement();
                }

                curGroup.transformations.push_back(new_tf);
            }
            else {
                double x = elem->DoubleAttribute("X");
                double y = elem->DoubleAttribute("Y");
                double z = elem->DoubleAttribute("Z");
                double trans[16] = {1,0,0,0,
                                    0,1,0,0,
                                    0,0,1,0,
                                    x,y,z,1};
                double result[16];
                multiply(matrix, trans, result);
                matrix = result;
            }
        }
        else if(!strcmp(elem->Name(), "rotate")) {
            if(!hasRotate) hasRotate = true;
            else {
                puts("Error - too many rotations applied to the same group.");
                exit(1);
            }
            if (elem->QueryDoubleAttribute("time", &time) == XML_SUCCESS) {
                transformation tf;
                memcpy(&tf.matrix, matrix, 16 * sizeof(double));
                tf.time = -1;
                curGroup.transformations.push_back(tf);
                double idM[16] = {1,0,0,0,
                                0,1,0,0,
                                0,0,1,0,
                                0,0,0,1};
                matrix = idM;

                transformation new_tf;
                new_tf.time = time;

                double x = elem->DoubleAttribute("axisX");
                double y = elem->DoubleAttribute("axisY");
                double z = elem->DoubleAttribute("axisZ");
                new_tf.points.push_back({x,y,z});

                curGroup.transformations.push_back(new_tf);
            }
            else {
                double angle = elem->DoubleAttribute("angle");
                double axisX = elem->DoubleAttribute("axisX");
                double axisY = elem->DoubleAttribute("axisY");
                double axisZ = elem->DoubleAttribute("axisZ");
                double c = cos(angle * 2 * M_PI / 360);
                double s = sin(angle * 2 * M_PI / 360);
                double rotate[16] = {axisX*axisX*(1-c)+c,      axisY*axisX*(1-c)+axisZ*s,axisX*axisZ*(1-c)-axisY*s,0,
                                    axisX*axisY*(1-c)-axisZ*s,axisY*axisY*(1-c)+c,      axisY*axisZ*(1-c)+axisX*s,0,
                                    axisX*axisZ*(1-c)+axisY*s,axisY*axisZ*(1-c)-axisX*s,axisZ*axisZ*(1-c)+c,      0,
                                    0,                        0,                        0,                        1};
                double result[16];
                multiply(matrix, rotate, result);
                matrix = result;
            }
        }
        else if(!strcmp(elem->Name(), "scale")) {
            if(!hasScale) hasScale = true;
            else {
                puts("Error - too many scales applied to the same group.");
                exit(1);
            }
            double x = 1, y = 1, z = 1;
            elem->QueryDoubleAttribute("X",&x);
            elem->QueryDoubleAttribute("Y",&y);
            elem->QueryDoubleAttribute("Z",&z);
            double scale[16] = {x,0,0,0,
                                0,y,0,0,
                                0,0,z,0,
                                0,0,0,1};
            double result[16];
            multiply(matrix, scale, result);
            matrix = result;
        }        
        else if(!strcmp(elem->Name(), "color")) {
            int red = elem->IntAttribute("R");
            int green = elem->IntAttribute("G");
            int blue = elem->IntAttribute("B");
            int newColors[3] = {red, green, blue};
            memcpy(&curGroup.colors, newColors, 3 * sizeof(int));
        }
        else if(!strcmp(elem->Name(), "group")) {
            parseGroup(scene, elem, matrix, curGroup);
        }
        elem = elem->NextSiblingElement();
    }
    
    if(hasModels) {
        struct group groupCopy = curGroup;

        transformation tf;
        memcpy(&tf.matrix, matrix, 16 * sizeof(double));
        tf.time = -1;
        groupCopy.transformations.push_back(tf);

        groupCopy.solids = solids;
        groupCopy.solidAttributes = attributes;
        solids.clear();
        int n = 0;
        for(size_t solid = 0; solid < groupCopy.solids.size(); solid++) {
            n += groupCopy.solids.at(solid)[0].size();
        }
        groupCopy.numVertices = n;

        scene.push_back(groupCopy);
    }
}