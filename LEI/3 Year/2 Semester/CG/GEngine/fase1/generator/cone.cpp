#include <vector>
#include "point.hpp"

#define _USE_MATH_DEFINES
#include <math.h>

std::vector<Point> drawCone(double radius, double height, int slices, int stacks) {
    std::vector<Point> cone;

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

            /*
            p1---p4
            |    |
            |    |
            p2---p3
            */

            cone.push_back(p2);
            cone.push_back(p3);
            cone.push_back(p4);

            if (stack != stacks) {
                cone.push_back(p1);
                cone.push_back(p2);
                cone.push_back(p4);
            }
        }
    }

    Point px = Point(0,0,0);

    for(int slice = 0; slice < slices; slice++) {
        double alpha = slice * sliceStep;
        double alphaX = (slice + 1) * sliceStep;

        Point pa = Point(radius * sin(alpha), 0, radius * cos(alpha));
        Point pb = Point(radius * sin(alphaX), 0, radius * cos(alphaX));

        cone.push_back(px);
        cone.push_back(pb);
        cone.push_back(pa);
    }

    return cone;
}