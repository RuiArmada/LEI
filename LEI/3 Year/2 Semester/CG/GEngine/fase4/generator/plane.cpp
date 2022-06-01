#include <vector>
#include <array>
#include "point.hpp"

std::array<std::vector<Point>,3> drawPlane(double side) {
    std::vector<Point> coords;
    std::vector<Point> normals;
    std::vector<Point> textures;

    double half = side / 2;

    Point p1 = Point(-half, 0, -half);
    Point p2 = p1.invertZ();
    Point p3 = p2.invertX();
    Point p4 = p1.invertX();

    Point n1 = Point(0 , 1 , 0);
    Point n2 = Point(0 , 1 , 0);
    Point n3 = Point(0 , 1 , 0);
    Point n4 = Point(0 , 1 , 0);
    Point n5 = Point(0 , 1 , 0);
    Point n6 = Point(0 , 1 , 0);

    Point t1 = Point(0 , 0 , 0);
    Point t2 = Point(1 , 0 , 0);
    Point t3 = Point(0 , 1 , 0);
    Point t4 = Point(1 , 0 , 0);
    Point t5 = Point(1 , 1 , 0);
    Point t6 = Point(0 , 1 , 0);

    // first triangle

    coords.push_back(p1);
    coords.push_back(p2);
    coords.push_back(p4);

    // second triangle

    coords.push_back(p2);
    coords.push_back(p3);
    coords.push_back(p4);

    // normals
    normals.push_back(n1);
    normals.push_back(n2);
    normals.push_back(n3);
    normals.push_back(n4);
    normals.push_back(n5);
    normals.push_back(n6);

    // textures
    textures.push_back(t1);
    textures.push_back(t2);
    textures.push_back(t3);
    textures.push_back(t4);
    textures.push_back(t5);
    textures.push_back(t6);

    std::array<std::vector<Point>,3> result = {coords, normals, textures};
    return result;
}
