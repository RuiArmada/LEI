#include <vector>
#include <array>
#include <math.h>
#include "point.hpp"

std::array<std::vector<Point>,3> drawSphere(double radius, int slices, int stacks) {
    std::vector<Point> coords;
    std::vector<Point> normals;
    std::vector<Point> textures;

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

            Point n1 = p1.normalize();
            Point n2 = p2.normalize();
            Point n3 = p3.normalize();
            Point n4 = p4.normalize();

            Point t1 = Point((double) slice / slices      , (double) stack / stacks      , 0);
            Point t2 = Point((double) slice / slices      , (double) (stack + 1) / stacks, 0);
            Point t3 = Point((double) (slice + 1) / slices, (double) (stack + 1) / stacks, 0);
            Point t4 = Point((double) (slice + 1) / slices, (double) stack / stacks      , 0);

            /*
            p1---p4
            |    |
            |    |
            p2---p3

            Na primeira stack p1 == p4 e na última p2 == p3.
            */

            // first triangle
            if (stack != 0) {
                coords.push_back(p1);
                coords.push_back(p2);
                coords.push_back(p4);

                normals.push_back(n1);
                normals.push_back(n2);
                normals.push_back(n4);

                textures.push_back(t1);
                textures.push_back(t2);
                textures.push_back(t4);
            }

            // second triangle
            if (stack != stacks - 1) {
                coords.push_back(p2);
                coords.push_back(p3);
                coords.push_back(p4);

                normals.push_back(n2);
                normals.push_back(n3);
                normals.push_back(n4);

                textures.push_back(t2);
                textures.push_back(t3);
                textures.push_back(t4);
            }
        }
    }
    std::array<std::vector<Point>,3> result = {coords, normals, textures};
    return result;
}
