#ifndef PARSER_HPP
#define PARSER_HPP

#include <vector>
#include <array>
#include <string>

#include "point.hpp"
#include "tinyxml2.h"

using std::vector;
using std::array;

struct transformation {
    double time;
    vector<array<double,3>> points;
    double matrix[16];
    double prev_y[3] = {0,1,0};
};

struct solidAttribs {
    array<float, 4> diffuse; bool hasDiffuse = false;
    array<float, 4> specular; bool hasSpecular = false;
    array<float, 4> emissive; bool hasEmissive = false;
    array<float, 4> ambient; bool hasAmbient = false;
    std::string textureFile;
};


struct group {
    vector<array<vector<Point>,3>> solids;
    int numVertices;
    int colors[3];
    vector<transformation> transformations;
    vector<solidAttribs> solidAttributes;
};


array<vector<Point>,3> vectorize(const char *filename);
void parseGroup(vector<struct group> & scene, tinyxml2::XMLElement* group, double * matrix, struct group curGroup);

#endif