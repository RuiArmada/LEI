#include <algorithm>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>

#define _USE_MATH_DEFINES
#include <math.h>

#include "point.hpp"

float* formulae(float t, float* point1, float* point2, float* point3, float* point4) {
    float aux = 1.0 -t;
    float *result = new float[3];
    float pt1 = aux * aux * aux;
    float pt2 = 3 * (aux * aux) * t;
    float pt3 = 3 * aux * (t * t);
    float pt4 = t * t * t;

    result[0] = (pt1 * point1[0]) + (pt2 * point2[0]) + (pt3 * point3[0]) + (pt4 * point4[0]);
    result[1] = (pt1 * point1[1]) + (pt2 * point2[1]) + (pt3 * point3[1]) + (pt4 * point4[1]);
    result[2] = (pt1 * point1[2]) + (pt2 * point2[2]) + (pt3 * point3[2]) + (pt4 * point4[2]);

    return result;
}

float* bezier(float a, float b, float** points, int* indice) {
    float* pt = new float[3];
    float alt[4][3];
    float res[4][3];
    int j = 0, x = 0;
    float *result;
    for(int i = 0 ; i < 16 ; i++) {
        alt[j][0] = points[indice[i]][0];
        alt[j][1] = points[indice[i]][1];
        alt[j][2] = points[indice[i]][2];
        j++;
        if(j % 4 == 0) {
            pt = formulae(a , alt[0] , alt[1] , alt[2] , alt[3]);
            res[x][0] = pt[0];
            res[x][1] = pt[1];
            res[x][2] = pt[2];
            x++;
            j = 0;
        }
    }
    result = formulae(b, res[0], res[1], res[2], res[3]);
    return result;
}

std::vector<Point> patch(std::string file , int tess) {
    std::vector<Point> bezierVector;
    std::ifstream read(file);
    std::string line, aux;
    int pos;
    float inc = 1.0 / tess;
    if(read.is_open()) {
        getline(read , line);
        int numPatch = atoi(line.c_str());
        int** index = new int*[numPatch];
        std::cout << "Number of Patch: " << numPatch << std::endl;
        float*** ptResult =  new float**[numPatch];
        for(int i1 = 0 ; i1 < numPatch ; i1++) {
            getline(read , line);
            index[i1] = new int[16];
            for(int j1 = 0 ; j1 < 16 ; j1++) {
                pos = line.find(",");
                aux = line.substr(0 , pos);
                index[i1][j1] = atoi(line.c_str());
                std::cout << "Index: " << index[i1][j1] << std::endl;
                line.erase(0 , pos + 1);
            }
        }
        getline(read , line);
        int controlPoints = atoi(line.c_str());
        float** points = new float*[controlPoints];
        for(int i2 = 0 ; i2 < controlPoints ; i2++) {
            getline(read , line);
            points[i2] = new float[3];
            for(int j2 = 0 ; j2 < 3 ; j2++) {
                pos = line.find(",");
                aux = line.substr(0 , pos);
                points[i2][j2] = atof(aux.c_str());
                line.erase(0 , pos + 1);
            }
        }
        for(int i3 = 0 ; i3 < numPatch ; i3++) {
            ptResult[i3] = new float*[4];
            for(int x = 0 ; x < tess ; x++) {
                for(int y = 0 ; y < tess ; y++) {
                    float x1 = inc * x;
                    float x2 = inc * (x + 1);
                    float y1 = inc * y;
                    float y2 = inc * (y + 1);

                    ptResult[i3][0] = bezier(x1 , y1 , points , index[i3]);
                    ptResult[i3][1] = bezier(x1 , y2 , points , index[i3]);
                    ptResult[i3][2] = bezier(x2 , y1 , points , index[i3]);
                    ptResult[i3][3] = bezier(x2 , y2 , points , index[i3]);

                    Point p1 = Point(ptResult[i3][0][0],ptResult[i3][0][1],ptResult[i3][0][2]);
                    Point p2 = Point(ptResult[i3][2][0],ptResult[i3][2][1],ptResult[i3][2][2]);
                    Point p3 = Point(ptResult[i3][3][0],ptResult[i3][3][1],ptResult[i3][3][2]);

                    Point p4 = Point(ptResult[i3][0][0],ptResult[i3][0][1],ptResult[i3][0][2]);
                    Point p5 = Point(ptResult[i3][3][0],ptResult[i3][3][1],ptResult[i3][3][2]);
                    Point p6 = Point(ptResult[i3][1][0],ptResult[i3][1][1],ptResult[i3][1][2]);

                    bezierVector.push_back(p1);
                    bezierVector.push_back(p2);
                    bezierVector.push_back(p3);
                    bezierVector.push_back(p4);
                    bezierVector.push_back(p5);
                    bezierVector.push_back(p6);
                }
            }
        }
     
        read.close();
        return bezierVector;
    }
    
    else
        std::cout << "Invalid file" << std::endl;
}
