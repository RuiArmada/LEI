#include <vector>
#include <math.h>
#include "point.hpp"

std::vector<Point> drawSphere(double radius, int slices, int stacks) {
    std::vector<Point> sphere;

    double pi = acos(-1);

    double sliceStep = 2 * pi / slices;
    double stackStep = pi / stacks;
    double sliceAngle, stackAngle, sliceAngleN, stackAngleN;

    for(int stack = 0; stack < stacks; stack++) {

        stackAngle = pi / 2 - stack * stackStep;
        stackAngleN = pi / 2 - (stack + 1) * stackStep;

        for(int slice = 0; slice < slices; slice++) {

            sliceAngle = slice * sliceStep; 
            sliceAngleN = (slice + 1) * sliceStep;

            Point p1 = Point( radius * cos(stackAngle) * sin(sliceAngle), radius * sin(stackAngle), radius * cos(stackAngle) * cos(sliceAngle) );
            Point p2 = Point( radius * cos(stackAngleN) * sin(sliceAngle), radius * sin(stackAngleN), radius * cos(stackAngleN) * cos(sliceAngle) );
            Point p3 = Point( radius * cos(stackAngleN) * sin(sliceAngleN), radius * sin(stackAngleN), radius * cos(stackAngleN) * cos(sliceAngleN) );
            Point p4 = Point( radius * cos(stackAngle) * sin(sliceAngleN), radius * sin(stackAngle), radius * cos(stackAngle) * cos(sliceAngleN) );

            /*
            p1---p4
            |    |
            |    |
            p2---p3

            Na primeira stack p1 == p4 e na última p2 == p3.
            */

            // first triangle
            if (stack != 0) {
                sphere.push_back(p1);
                sphere.push_back(p2);
                sphere.push_back(p4);
            }

            // second triangle
            if (stack != stacks - 1) {
                sphere.push_back(p2);
                sphere.push_back(p3);
                sphere.push_back(p4);
            }
        }
    }
    return sphere;
}
