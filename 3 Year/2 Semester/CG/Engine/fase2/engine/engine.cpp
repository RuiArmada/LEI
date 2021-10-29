#include <stdlib.h>

#ifdef _APPLE_
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#define _USE_MATH_DEFINES
#include <math.h>

#include <stdio.h>
#include <fstream>
#include <string>
#include <vector>
#include <array>

#include "tinyxml2.h"
#include "point.hpp"

using namespace tinyxml2;

XMLDocument doc;

struct group {
    std::vector<std::vector<Point>> solids;
    GLdouble matrix[16];
    int colors[3];
};

std::vector<group> scene;

double alpha = 0, beta = M_PI_4, radius = 100.0f;
double camX, camY, camZ;
double dirX, dirY, dirZ;

void spherical2Cartesian() {

	camX = radius * cos(beta) * sin(alpha);
	camY = radius * sin(beta);
	camZ = radius * cos(beta) * cos(alpha);
}

bool FPS = false;

//----------------------------------------------------------------------------

void changeSize (int width, int height){
    //Prevent a divide by zero, when window is too short
    //(you can't make a window with zero width)
    if(height == 0)
        height = 1;
    //compute window's aspect ratio
    float ratio = width * 1.0 /height;
    //Set the projection matrix as current
    glMatrixMode(GL_PROJECTION);
    //Load Identity Matrix
    glLoadIdentity();
    //Set the viewport to be the entire window
    glViewport(0, 0, width, height);
    //Set perspective
    gluPerspective(45.0f, ratio, 1.0f, 1000.0f);
    //return to the model view matrix mode
    glMatrixMode(GL_MODELVIEW);
}


void draw_axis(){
    glBegin(GL_LINES);
    //X axis in red
    glColor3f(1.0f, 0.0f, 0.0f);
    glVertex3f( 0.0f, 0.0f, 0.0f);
    glVertex3f( 100.0f, 0.0f, 0.0f);
    //Y Axis in Green
    glColor3f(0.0f, 1.0f, 0.0f);
    glVertex3f(0.0f, 0.0f, 0.0f);
    glVertex3f(0.0f, 100.0f, 0.0f);
    //Z Axis in Blue
    glColor3f(0.0f, 0.0f, 1.0f);
    glVertex3f(0.0f, 0.0f, 0.0f);
    glVertex3f(0.0f, 0.0f, 100.0f);
    glEnd();    
}

std::vector<Point> vectorize(const char *filename) {

    std::ifstream file(filename);
    if(!file.good()) {
        file.open(std::string("../").append(filename).c_str());
        if(!file.good()) {
            printf("Error opening file %s\n", filename);
            exit(1);
        }
    }

    std::string line;
    std::vector<Point> solid;

    printf("Reading %s\n", filename);
    std::getline(file,line);

    unsigned long N = std::stoul(line);
    solid.reserve(N);
    for (unsigned long i = 0; i < N; i++) {
        std::getline(file, line);
        double a, b, c;
        int matches = sscanf(line.c_str(), "%lf,%lf,%lf", &a, &b, &c);
        if (matches != 3) {
            printf("ERROR - invalid number of points in vertex %s\n", line.c_str());
            exit(1);
        }
        solid.push_back(Point(a, b, c));
    }

    file.close();
    printf("Finished reading %s\n", filename);
    return solid;
}

void multiply(double * mat1,
              double * mat2,
              double * res) {
    int i, j, k;
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            res[i * 4 + j] = 0;
            for (k = 0; k < 4; k++)
                res[i * 4 + j] += mat1[i * 4 + k] * mat2[k * 4 + j];
        }
    }
}

void parseGroup(XMLElement* group, GLdouble * matrix, int * colors) {
    bool hasTrans = false, hasRotate = false, hasScale = false, hasModels = false;
    std::vector<std::vector<Point>> solids;
    XMLElement* elem = group->FirstChildElement();
    while(elem) {
        if(!strcmp(elem->Name(), "models")) {
            if(!hasModels) hasModels = true;
            else {
                puts("Error - too many models in the same group.");
                exit(1);
            }
            XMLElement* model = elem->FirstChildElement();
            while(model) {
                solids.push_back(vectorize(model->Attribute("file")));
                model = model->NextSiblingElement();
            }
        }
        else if(!strcmp(elem->Name(), "translate")) {
            if(!hasTrans) hasTrans = true;
            else {
                puts("Error - too many transformations applied to the same group.");
                exit(1);
            }
            double x = elem->DoubleAttribute("X");
            double y = elem->DoubleAttribute("Y");
            double z = elem->DoubleAttribute("Z");
            double trans[16] = {1,0,0,0,
                                0,1,0,0,
                                0,0,1,0,
                                x,y,z,1};
            double result[16];
            multiply(matrix, trans, result);
            matrix = result;
        }
        else if(!strcmp(elem->Name(), "rotate")) {
            if(!hasRotate) hasRotate = true;
            else {
                puts("Error - too many rotations applied to the same group.");
                exit(1);
            }
            double angle = elem->DoubleAttribute("angle");
            double axisX = elem->DoubleAttribute("axisX");
            double axisY = elem->DoubleAttribute("axisY");
            double axisZ = elem->DoubleAttribute("axisZ");
            double c = cos(angle * 2 * M_PI / 360);
            double s = sin(angle * 2 * M_PI / 360);
            double rotate[16] = {axisX*axisX*(1-c)+c,      axisY*axisX*(1-c)+axisZ*s,axisX*axisZ*(1-c)-axisY*s,0,
                                 axisX*axisY*(1-c)-axisZ*s,axisY*axisY*(1-c)+c,      axisY*axisZ*(1-c)+axisX*s,0,
                                 axisX*axisZ*(1-c)+axisY*s,axisY*axisZ*(1-c)-axisX*s,axisZ*axisZ*(1-c)+c,      0,
                                 0,                        0,                        0,                        1};
            double result[16];
            multiply(matrix, rotate, result);
            matrix = result;
        }
        else if(!strcmp(elem->Name(), "scale")) {
            if(!hasScale) hasScale = true;
            else {
                puts("Error - too many scales applied to the same group.");
                exit(1);
            }
            double x = 1, y = 1, z = 1;
            elem->QueryDoubleAttribute("X",&x);
            elem->QueryDoubleAttribute("Y",&y);
            elem->QueryDoubleAttribute("Z",&z);
            double scale[16] = {x,0,0,0,
                                0,y,0,0,
                                0,0,z,0,
                                0,0,0,1};
            double result[16];
            multiply(matrix, scale, result);
            matrix = result;
        }        
        else if(!strcmp(elem->Name(), "color")) {
            int red = elem->IntAttribute("R");
            int green = elem->IntAttribute("G");
            int blue = elem->IntAttribute("B");
            int newColors[3] = {red, green, blue};
            colors = newColors;
        }
        else if(!strcmp(elem->Name(), "group")) {
            parseGroup(elem, matrix, colors);
        }
        elem = elem->NextSiblingElement();
    }
    
    if(hasModels) {
        struct group x;
        x.solids = solids;
        memcpy(&x.colors, colors, 3 * sizeof(int));
        memcpy(&x.matrix, matrix, 16 * sizeof(double));

        scene.push_back(x);
    }
}

void renderScene(){ //this function reads and draws the XML scene
    //buffer reset
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glLoadIdentity();
    if(FPS) {
	    gluLookAt(camX, camY, camZ,
		      camX + dirX, camY + dirY, camZ + dirZ,
			  0.0f,beta > M_PI_2 ? -1.0f : 1.0f,0.0f);
    }
    else {
        gluLookAt(camX, camY, camZ,
		      0, 0, 0,
			  0.0f,beta > M_PI_2 ? -1.0f : 1.0f,0.0f);
    }

    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

    for (unsigned long group = 0; group < scene.size(); group++) {
        std::vector<std::vector<Point>> solids = scene[group].solids;
        for (unsigned long i = 0; i < solids.size(); i++) {
            glPushMatrix();
            std::vector<Point> solid = solids[i];
            glMultMatrixd(scene[group].matrix);
            glColor3ub(scene[group].colors[0],scene[group].colors[1],scene[group].colors[2]);
            glBegin(GL_TRIANGLES);
                for (unsigned long j = 0; j < solid.size(); j++)
                    glVertex3d(solid[j].x(), solid[j].y(), solid[j].z());
            glEnd();
            glPopMatrix();
        }
    }

    glutSwapBuffers();
}

void keyboardFunc(unsigned char key, int x, int y) {
    if (key == 'v') {
        double n = sqrt(camX * camX + camY * camY + camZ * camZ);
        if(!FPS) {
            dirX = - camX / n;
            dirY = - camY / n;
            dirZ = - camZ / n;
        }
        else {
            alpha = atan2(camX, camZ);
            beta = asin(camY / n);
            radius = n;
        }
        FPS = !FPS;
    }
    if(!FPS) {
        switch(key) {
            case 'a':
                alpha -= M_PI / 16;
                break;
            case 'd':
                alpha += M_PI / 16;
                break;
            case 's':
                beta -= M_PI / 16;
                break;
            case 'w':
                beta += M_PI / 16;
                break;
            case 'q':
                if (radius > 1) radius -= 1;
                break;
            case 'e':
                radius += 1;
                break;
        }
        if (alpha < 0) alpha += M_PI * 2;
        else if (alpha > M_PI * 2) alpha -= M_PI * 2;
        if (beta < - M_PI_2) beta += M_PI * 2;
        else if (beta > (3 * M_PI_2)) beta -= M_PI * 2;
        spherical2Cartesian();
    }
    else {
        double newX = -dirZ * (beta > M_PI_2 ? -1.0f : 1.0f);
        double newZ =  dirX * (beta > M_PI_2 ? -1.0f : 1.0f);
        double n = sqrt(newX * newX + newZ * newZ);
        switch(key) {
            case 'a':
                camX -= 0.5 * newX / (n == 0 ? 1 : n);
                camZ -= 0.5 * newZ / (n == 0 ? 1 : n);
                break;
            case 'd':
                camX += 0.5 * newX / (n == 0 ? 1 : n);
                camZ += 0.5 * newZ / (n == 0 ? 1 : n);
                break;
            case 's':
                camX -= 0.5 * dirX;
                camY -= 0.5 * dirY;
                camZ -= 0.5 * dirZ;
                break;
            case 'w':
                camX += 0.5 * dirX;
                camY += 0.5 * dirY;
                camZ += 0.5 * dirZ;
                break;
        }
    }
	glutPostRedisplay();
}

bool firstMouse = true, move = false;
int oldx, oldy;
double yaw, pitch;

void mouseFunc(int button, int state, int x, int y) {
    if(button == GLUT_LEFT_BUTTON) {
        if (state == GLUT_DOWN) {
            pitch = 360 * asin(dirY) / (2 * M_PI);
            yaw = 360 * atan2(dirZ, dirX) / (2 * M_PI);
            firstMouse = true;
            move = true;
        }
        else {
            move = false;
        }
    }
}

void motionFunc(int x, int y) {
    if(FPS && move) {
        if(firstMouse) {
            oldx = x;
            oldy = y;
            firstMouse = false;
            return;
        }

        int deltax = x - oldx;
        int deltay = oldy - y;

        oldx = x;
        oldy = y;

        double sensitivity = 0.1;

        yaw   += deltax * sensitivity;
        pitch += deltay * sensitivity;
                    
        if(pitch > 89.0f)
            pitch = 89.0f;
        if(pitch < -89.0f)
            pitch = -89.0f;

        dirX = cos(pitch * 2 * M_PI / 360) * cos(yaw * 2 * M_PI / 360);
        dirY = sin(pitch * 2 * M_PI / 360);
        dirZ = cos(pitch * 2 * M_PI / 360) * sin(yaw * 2 * M_PI / 360);

        double n = sqrt(dirX * dirX + dirY * dirY + dirZ * dirZ);
        dirX /= n;
        dirY /= n;
        dirZ /= n;
        
        glutPostRedisplay();
    }
}

int main(int argc, char** argv) {

    if(argc != 2){
         puts("Too few arguments, please insert the correct number of arguments");
         return 1;
    }

    doc.LoadFile(argv[1]);
    if(doc.ErrorID()) {
        doc.LoadFile(std::string("../").append(argv[1]).c_str());
        if(doc.ErrorID()) {
            printf("%s\n", doc.ErrorStr());
            return doc.ErrorID();
        }
    }

    XMLElement* scene = doc.FirstChildElement("scene");
    if(scene == NULL) {
        puts("No scene found.");
        return 1;
    }

    GLdouble idM[] = {1,0,0,0,
                      0,1,0,0,
                      0,0,1,0,
                      0,0,0,1};

    int colors[3] = {255, 255, 255};

    parseGroup(scene, idM, colors);

    //init GLUT and the Window
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
    glutInitWindowPosition(100,100);
    glutInitWindowSize(800,800);
    glutCreateWindow("Projeto CG");

    //Required callback registry
    glutDisplayFunc(renderScene);
    glutKeyboardFunc(keyboardFunc);
	glutReshapeFunc(changeSize);
    glutMouseFunc(mouseFunc);
    glutMotionFunc(motionFunc);

    //OpenGL settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    spherical2Cartesian();

    //enter GLUT's main cycle
    glutMainLoop();

    return 0;
}