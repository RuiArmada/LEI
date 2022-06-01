#include <vector>
#define _USE_MATH_DEFINES
#include <math.h>
#include "point.hpp"


std::vector<Point> drawTorus(double innerRadius, double outerRadius, int slices, int stacks) {
    std::vector<Point> torus;

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

            torus.push_back(p1);
            torus.push_back(p2);
            torus.push_back(p4);

            torus.push_back(p2);
            torus.push_back(p3);
            torus.push_back(p4);
        }
    }
    return torus;
}