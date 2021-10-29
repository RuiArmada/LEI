#include <vector>
#include "point.hpp" 

std::vector<Point> drawBoxD(float length, float width, float height, int divisions) {
    std::vector<Point> cube;

    float step_x = length / divisions;
    float step_y = height / divisions;
    float step_z = width / divisions;

    float init_x = length / 2.0;
    float init_y = height / 2.0;
    float init_z = width / 2.0;

    for(int i = 0; i < divisions; i++) {
        for(int j = 0; j < divisions; j++) {

            // face 1 (bottom)

            Point p1 = Point(-init_x + i * step_x, -init_y, -init_z + j * step_z);
            Point p2 = Point(-init_x + (i + 1) * step_x, -init_y, -init_z + j * step_z);
            Point p3 = Point(-init_x + (i + 1) * step_x, -init_y, -init_z + (j + 1) * step_z);
            Point p4 = Point(-init_x + i * step_x, -init_y, -init_z + (j + 1) * step_z);

            // first triangle

            cube.push_back(p1);
            cube.push_back(p2);
            cube.push_back(p4);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p3);
            cube.push_back(p4);

            // face 2 (top)

            p1 = p1.invertY();
            p2 = p2.invertY();
            p3 = p3.invertY();
            p4 = p4.invertY();

            // first triangle

            cube.push_back(p1);
            cube.push_back(p4);
            cube.push_back(p2);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p4);
            cube.push_back(p3);

            // face 3 (front)

            p1 = Point(-init_x + i * step_x, -init_y + j * step_y, init_z);
            p2 = Point(-init_x + (i + 1) * step_x, -init_y + j * step_y, init_z);
            p3 = Point(-init_x + (i + 1) * step_x, -init_y + (j + 1) * step_y, init_z);
            p4 = Point(-init_x + i * step_x, -init_y + (j + 1) * step_y, init_z);

            // first triangle

            cube.push_back(p1);
            cube.push_back(p2);
            cube.push_back(p4);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p3);
            cube.push_back(p4);

            // face 4 (back)

            p1 = p1.invertZ();
            p2 = p2.invertZ();
            p3 = p3.invertZ();
            p4 = p4.invertZ();

            // first triangle

            cube.push_back(p1);
            cube.push_back(p4);
            cube.push_back(p2);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p4);
            cube.push_back(p3);

            // face 5 (left)

            p1 = Point(-init_x, -init_y + i * step_y, -init_z + j * step_z);
            p2 = Point(-init_x, -init_y + (i + 1) * step_y, -init_z + j * step_z);
            p3 = Point(-init_x, -init_y + (i + 1) * step_y, -init_z + (j + 1) * step_z);
            p4 = Point(-init_x, -init_y + i * step_y, -init_z + (j + 1) * step_z);

            // first triangle

            cube.push_back(p1);
            cube.push_back(p4);
            cube.push_back(p2);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p4);
            cube.push_back(p3);

            // face 6 (right)

            p1 = p1.invertX();
            p2 = p2.invertX();
            p3 = p3.invertX();
            p4 = p4.invertX();

            // first triangle

            cube.push_back(p1);
            cube.push_back(p2);
            cube.push_back(p4);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p3);
            cube.push_back(p4);
        }
    }
    return cube;
}

std::vector<Point> drawBox(float length, float width, float height) {
    int divisions = 1;
    return drawBoxD(length, width, height, divisions);
}