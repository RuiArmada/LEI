#include <vector>
#include <array>
#define _USE_MATH_DEFINES
#include <math.h>
#include "point.hpp"

// 0.6 1 50 50 

std::array<std::vector<Point>,3> drawTorus(double innerRadius, double outerRadius, int slices, int stacks) {
    std::vector<Point> coords;
    std::vector<Point> normals;
    std::vector<Point> textures;

    double radius = (innerRadius + outerRadius) / 2;
    double dist = radius - innerRadius;

    double sliceStep = (2*M_PI)/slices;
    double stackStep = (2*M_PI)/stacks;

    for(int slice = 0; slice < slices; slice++){

        for(int stack = 0; stack < stacks; stack++){
            
            Point p1 = Point((radius + dist * cos(slice * sliceStep))       * cos(stack * stackStep)      , dist * sin(slice * sliceStep)      , (radius + dist * cos(slice * sliceStep))       * sin(stack * stackStep));
            Point p2 = Point((radius + dist * cos((slice + 1) * sliceStep)) * cos(stack * stackStep)      , dist * sin((slice + 1) * sliceStep), (radius + dist * cos((slice + 1) * sliceStep)) * sin(stack * stackStep));
            Point p3 = Point((radius + dist * cos((slice + 1) * sliceStep)) * cos((stack + 1) * stackStep), dist * sin((slice + 1) * sliceStep), (radius + dist * cos((slice + 1) * sliceStep)) * sin((stack + 1) * stackStep));
            Point p4 = Point((radius + dist * cos(slice * sliceStep))       * cos((stack + 1) * stackStep), dist * sin(slice * sliceStep)      , (radius + dist * cos(slice * sliceStep))       * sin((stack + 1) * stackStep));

            Point n1 = Point(cos(slice * sliceStep)       * cos(stack * stackStep)      , sin(slice * sliceStep)      , cos(slice * sliceStep)       * sin(stack * stackStep)).normalize();
            Point n2 = Point(cos((slice + 1) * sliceStep) * cos(stack * stackStep)      , sin((slice + 1) * sliceStep), cos((slice + 1) * sliceStep) * sin(stack * stackStep)).normalize();
            Point n3 = Point(cos((slice + 1) * sliceStep) * cos((stack + 1) * stackStep), sin((slice + 1) * sliceStep), cos((slice + 1) * sliceStep) * sin((stack + 1) * stackStep)).normalize();
            Point n4 = Point(cos(slice * sliceStep)       * cos((stack + 1) * stackStep), sin(slice * sliceStep)      , cos(slice * sliceStep)       * sin((stack + 1) * stackStep)).normalize();

            Point t1 = Point((double) slice / slices      , (double) stack / stacks      , 0);
            Point t2 = Point((double) (slice + 1) / slices, (double) stack / stacks      , 0);
            Point t3 = Point((double) (slice + 1) / slices, (double) (stack + 1) / stacks, 0);
            Point t4 = Point((double) slice / slices      , (double) (stack + 1) / stacks, 0);

            coords.push_back(p1);
            coords.push_back(p2);
            coords.push_back(p4);

            normals.push_back(n1);
            normals.push_back(n2);
            normals.push_back(n4);

            textures.push_back(t1);
            textures.push_back(t2);
            textures.push_back(t4);

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
    std::array<std::vector<Point>,3> result = {coords, normals, textures};
    return result;
}