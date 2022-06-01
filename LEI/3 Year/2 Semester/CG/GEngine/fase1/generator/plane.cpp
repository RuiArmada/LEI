#include <vector>
#include "point.hpp"

std::vector<Point> drawPlane(double side) {
    std::vector<Point> plane;
    double half = side / 2;

    Point p1 = Point(-half, 0, -half);
    Point p2 = p1.invertZ();
    Point p3 = p2.invertX();
    Point p4 = p1.invertX();

    // first triangle

    plane.push_back(p1);
    plane.push_back(p2);
    plane.push_back(p4);

    // second triangle

    plane.push_back(p2);
    plane.push_back(p3);
    plane.push_back(p4);

    return plane;
}
