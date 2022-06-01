#include <vector>
#include <array>
#include "point.hpp" 

std::array<std::vector<Point>,3> drawBoxD(float length, float width, float height, int divisions) {
    std::vector<Point> cube;
    std::vector<Point> normals;
    std::vector<Point> textures;

    float step_x = length / divisions;
    float step_y = height / divisions;
    float step_z = width / divisions;

    float init_x = length / 2.0;
    float init_y = height / 2.0;
    float init_z = width / 2.0;

    for(int i = 0; i < divisions; i++) {
        for(int j = 0; j < divisions; j++) {

            /* ------------------------- BOTTOM -------------------------- */

            Point p1 = Point(-init_x + i * step_x      , -init_y, -init_z + j * step_z);
            Point p2 = Point(-init_x + (i + 1) * step_x, -init_y, -init_z + j * step_z);
            Point p3 = Point(-init_x + (i + 1) * step_x, -init_y, -init_z + (j + 1) * step_z);
            Point p4 = Point(-init_x + i * step_x      , -init_y, -init_z + (j + 1) * step_z);

            Point n = Point(0.0f , -1.0f , 0.0f);

            Point t1 = Point((double) i    /divisions, (double) j    /divisions, 0);
            Point t2 = Point((double) (i+1)/divisions, (double) j    /divisions, 0);
            Point t3 = Point((double) (i+1)/divisions, (double) (j+1)/divisions, 0);
            Point t4 = Point((double) i    /divisions, (double) (j+1)/divisions, 0);

            // first triangle

            cube.push_back(p1);
            cube.push_back(p2);
            cube.push_back(p4);

            textures.push_back(t1);
            textures.push_back(t2);
            textures.push_back(t4);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p3);
            cube.push_back(p4);

            textures.push_back(t2);
            textures.push_back(t3);
            textures.push_back(t4);

            // normals

            for(int i = 0; i < 6; i++) normals.push_back(n);

            /* --------------------------- TOP --------------------------- */

            n = Point(0.0f , 1.0f , 0.0f);

            p1 = p1.invertY();
            p2 = p2.invertY();
            p3 = p3.invertY();
            p4 = p4.invertY();

            // first triangle

            cube.push_back(p1);
            cube.push_back(p4);
            cube.push_back(p2);

            textures.push_back(t1);
            textures.push_back(t4);
            textures.push_back(t2);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p4);
            cube.push_back(p3);

            textures.push_back(t2);
            textures.push_back(t4);
            textures.push_back(t3);

            // normals

            for(int i = 0; i < 6; i++) normals.push_back(n);

            /* -------------------------- FRONT -------------------------- */

            p1 = Point(-init_x + i * step_x, -init_y + j * step_y, init_z);
            p2 = Point(-init_x + (i + 1) * step_x, -init_y + j * step_y, init_z);
            p3 = Point(-init_x + (i + 1) * step_x, -init_y + (j + 1) * step_y, init_z);
            p4 = Point(-init_x + i * step_x, -init_y + (j + 1) * step_y, init_z);

            n = Point(0.0f , 0.0f , 1.0f);

            // first triangle

            cube.push_back(p1);
            cube.push_back(p2);
            cube.push_back(p4);

            textures.push_back(t1);
            textures.push_back(t2);
            textures.push_back(t4);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p3);
            cube.push_back(p4);

            textures.push_back(t2);
            textures.push_back(t3);
            textures.push_back(t4);

            // normals

            for(int i = 0; i < 6; i++) normals.push_back(n);

            /* -------------------------- BACK --------------------------- */

            n = Point(0.0f , 0.0f , -1.0f);

            p1 = p1.invertZ();
            p2 = p2.invertZ();
            p3 = p3.invertZ();
            p4 = p4.invertZ();

            // first triangle

            cube.push_back(p1);
            cube.push_back(p4);
            cube.push_back(p2);

            textures.push_back(t1);
            textures.push_back(t4);
            textures.push_back(t2);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p4);
            cube.push_back(p3);

            textures.push_back(t2);
            textures.push_back(t4);
            textures.push_back(t3);

            // normals

            for(int i = 0; i < 6; i++) normals.push_back(n);

            /* -------------------------- LEFT --------------------------- */

            p1 = Point(-init_x, -init_y + i * step_y, -init_z + j * step_z);
            p2 = Point(-init_x, -init_y + (i + 1) * step_y, -init_z + j * step_z);
            p3 = Point(-init_x, -init_y + (i + 1) * step_y, -init_z + (j + 1) * step_z);
            p4 = Point(-init_x, -init_y + i * step_y, -init_z + (j + 1) * step_z);

            n = Point(-1.0f , 0.0f , 0.0f);

            // first triangle

            cube.push_back(p1);
            cube.push_back(p4);
            cube.push_back(p2);

            textures.push_back(t1);
            textures.push_back(t4);
            textures.push_back(t2);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p4);
            cube.push_back(p3);

            textures.push_back(t2);
            textures.push_back(t4);
            textures.push_back(t3);

            // normals

            for(int i = 0; i < 6; i++) normals.push_back(n);

            /* -------------------------- RIGHT -------------------------- */

            n = Point(1.0f , 0.0f , 0.0f);

            p1 = p1.invertX();
            p2 = p2.invertX();
            p3 = p3.invertX();
            p4 = p4.invertX();

            // first triangle

            cube.push_back(p1);
            cube.push_back(p2);
            cube.push_back(p4);

            textures.push_back(t1);
            textures.push_back(t2);
            textures.push_back(t4);

            // second triangle

            cube.push_back(p2);
            cube.push_back(p3);
            cube.push_back(p4);

            textures.push_back(t2);
            textures.push_back(t3);
            textures.push_back(t4);

            // normals

            for(int i = 0; i < 6; i++) normals.push_back(n);

            /* ----------------------------------------------------------- */
        }
    }
    std::array<std::vector<Point>,3> result = {cube, normals, textures};
    return result;
}

std::array<std::vector<Point>,3> drawBox(float length, float width, float height) {
    int divisions = 1;
    return drawBoxD(length, width, height, divisions);
}