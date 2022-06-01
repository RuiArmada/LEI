#include <vector>
#include <array>
#include "point.hpp"

#define _USE_MATH_DEFINES
#include <math.h>

std::array<std::vector<Point>,3> drawCone(double radius, double height, int slices, int stacks) {
    std::vector<Point> cone;
    std::vector<Point> normals;
    std::vector<Point> textures;

    double sliceStep = M_PI * 2 / slices;
    double stackStep = (M_PI / 2) / stacks;

    double theta = atan(radius/height);

    for(int stack = stacks; stack > 0; stack--) {

        double beta = stackStep * stack;
        double betaX = stackStep * (stack - 1);

        double r = height * sin(theta) / sin(M_PI - theta - M_PI_2 + beta);
        double rX = height * sin(theta) / sin(M_PI - theta - M_PI_2 + betaX);

        for(int slice = 0; slice < slices; slice++) {

            double alpha = slice * sliceStep;
            double alphaX = (slice + 1) * sliceStep;

            Point p1 = Point(r * cos(beta) * sin(alpha), r * sin(beta), r * cos(beta) * cos(alpha));
            Point p2 = Point(rX * cos(betaX) * sin(alpha), rX * sin(betaX), rX * cos(betaX) * cos(alpha));
            Point p3 = Point(rX * cos(betaX) * sin(alphaX), rX * sin(betaX), rX * cos(betaX) * cos(alphaX));
            Point p4 = Point(r * cos(beta) * sin(alphaX), r * sin(beta), r * cos(beta) * cos(alphaX));

            double dis1 = sqrt(pow(p1.x(),2)+pow(p1.y()-height,2)+pow(p1.y(),2));
            double D1 = dis1 / cos(atan(radius/height));

            Point n1 = Point(p1.x(), p1.y() - (height - D1), p1.z()).normalize();

            double dis2 = sqrt(pow(p2.x(),2)+pow(p2.y()-height,2)+pow(p2.y(),2));
            double D2 = dis2 / cos(atan(radius/height));

            Point n2 = Point(p2.x(), p2.y() - (height - D2), p2.z()).normalize();

            double dis3 = sqrt(pow(p3.x(),2)+pow(p3.y()-height,2)+pow(p3.y(),2));
            double D3 = dis3 / cos(atan(radius/height));

            Point n3 = Point(p3.x(), p3.y() - (height - D3), p3.z()).normalize();

            double dis4 = sqrt(pow(p4.x(),2)+pow(p4.y()-height,2)+pow(p4.y(),2));
            double D4 = dis4 / cos(atan(radius/height));

            Point n4 = Point(p4.x(), p4.y() - (height - D4), p4.z()).normalize();

            // Note : The formulae are wrong, needs fixing
            Point t1 = Point((float) 0.4375 + (0.1875 / stacks) * (stacks - stack) * cos(alpha),  (float) 0.1875 + (0.1875 / stacks) * (stacks - stack) * sin(alpha) , 0.0f);
            Point t2 = Point((float) 0.4375 + (0.1875 / stacks) * (stacks - stack) * cos(alphaX), (float) 0.1875 + (0.1875 / stacks) * (stacks - stack) * sin(alphaX) , 0.0f);
            Point t3 = Point((float) 0.4375 + (0.1875 / stacks) * (stacks - (stack + 1)) * cos(alpha) , (float) 0.1875 + (0.1875 / stacks) * (stacks - (stack + 1)) * sin(alpha) , 0.0f);
            Point t4 = Point((float) 0.4375 + (0.1875 / stacks) * (stacks - (stack + 1)) * cos(alphaX) , (float) 0.1875 + (0.1875 / stacks) * (stacks - (stack + 1)) * sin(alphaX) , 0.0f); 

            /*
            p1---p4
            |    |
            |    |
            p2---p3
            */

            cone.push_back(p2);
            cone.push_back(p3);
            cone.push_back(p4);

            normals.push_back(n2);
            normals.push_back(n3);
            normals.push_back(n4);

            textures.push_back(t2);
            textures.push_back(t3);
            textures.push_back(t4);

            if (stack != stacks) {

                cone.push_back(p1);
                cone.push_back(p2);
                cone.push_back(p4);

                normals.push_back(n1);
                normals.push_back(n2);
                normals.push_back(n4);

                textures.push_back(t1);
                textures.push_back(t2);
                textures.push_back(t4);

            }

        }
    }

    Point px = Point(0,0,0);

    for(int slice = 0; slice < slices; slice++) {
        double alpha = slice * sliceStep;
        double alphaX = (slice + 1) * sliceStep;

        Point pa = Point(radius * sin(alpha), 0, radius * cos(alpha));
        Point pb = Point(radius * sin(alphaX), 0, radius * cos(alphaX));

        Point n1 = Point(0.0f , -1.0f , 0.0f);

        // The last two are wrong. Will check them out later
        // Note : The formulae are wrong, needs fixing
        Point tx = Point(0.8125, -1, 0); 
        Point tb = Point(0.8125 + 0.1875 * cos(alpha), 0.1875 + 0.1875 * sin(alpha) , 0);
        Point ta = Point(0.8125 + 0.1875 * cos(alphaX), 0.1875 + 0.1875 * sin(alphaX), 0);

        cone.push_back(px);
        cone.push_back(pb);
        cone.push_back(pa);

        textures.push_back(tx);
        textures.push_back(tb);
        textures.push_back(ta);

        // normals
        for(int i = 0; i < 3; i++) normals.push_back(n1);
    }

    std::array<std::vector<Point>,3> result = {cone, normals, textures};
    return result;
} 