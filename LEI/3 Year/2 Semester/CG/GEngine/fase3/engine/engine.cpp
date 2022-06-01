#include <stdlib.h>

#ifdef _APPLE_
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
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
#include "algebra.hpp"

using namespace tinyxml2;
using std::vector;

XMLDocument doc;

struct transformation {
    double time;
    vector<std::array<double,3>> points;
    GLdouble matrix[16];
    double prev_y[3] = {0,1,0};
};

struct group {
    vector<vector<Point>> solids;
    int numVertices;
    int colors[3];
    vector<transformation> transformations;
};

vector<group> scene;
vector<GLuint> buffers;

double alpha = -M_PI_4, beta = M_PI_4 / 2, radius = 100.0f;
double camX, camY, camZ;
double dirX, dirY, dirZ;

void spherical2Cartesian() {

	camX = radius * cos(beta) * sin(alpha);
	camY = radius * sin(beta);
	camZ = radius * cos(beta) * cos(alpha);
}

bool FPS = false, VBOs = true, filledSolids = false;

int timebase;
double frames;

void framerate() {
    char title[50];
    frames++;
    double time = glutGet(GLUT_ELAPSED_TIME);
    
    if (time - timebase> 1000) {
        double fps = frames * 1000.0 / (time - timebase);
        timebase = time;
        frames = 0;
        sprintf(title, "Projeto CG | %lf FPS", fps);
        glutSetWindowTitle(title);
    }
}

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

vector<Point> vectorize(const char *filename) {

    std::ifstream file(filename);
    if(!file.good()) {
        file.open(std::string("../").append(filename).c_str());
        if(!file.good()) {
            printf("Error opening file %s\n", filename);
            exit(1);
        }
    }

    std::string line;
    vector<Point> solid;

    //printf("Reading %s\n", filename);
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
    //printf("Finished reading %s\n", filename);
    return solid;
}

void parseGroup(XMLElement* group, GLdouble * matrix, int * colors, vector<transformation> transformations) {
    bool hasTrans = false, hasRotate = false, hasScale = false, hasModels = false;
    vector<vector<Point>> solids;
    double time = -1;

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
            if (elem->QueryDoubleAttribute("time", &time) == XML_SUCCESS) {
                transformation tf;
                memcpy(&tf.matrix, matrix, 16 * sizeof(double));
                tf.time = -1;
                transformations.push_back(tf);
                double idM[16] = {1,0,0,0,
                                0,1,0,0,
                                0,0,1,0,
                                0,0,0,1};
                matrix = idM;

                transformation new_tf;
                new_tf.time = time;

                XMLElement* child_elem = elem->FirstChildElement();
                while (child_elem) {
                    if(strcmp(child_elem->Name(),"point")) {
                        printf("Error - invalid element in translation - expected \"point\", got \"%s\"\n",child_elem->Name());
                        exit(1);
                    }
                    double x = child_elem->DoubleAttribute("X");
                    double y = child_elem->DoubleAttribute("Y");
                    double z = child_elem->DoubleAttribute("Z");
                    new_tf.points.push_back({x,y,z});
                    child_elem = child_elem->NextSiblingElement();
                }

                transformations.push_back(new_tf);
            }
            else {
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
        }
        else if(!strcmp(elem->Name(), "rotate")) {
            if(!hasRotate) hasRotate = true;
            else {
                puts("Error - too many rotations applied to the same group.");
                exit(1);
            }
            if (elem->QueryDoubleAttribute("time", &time) == XML_SUCCESS) {
                transformation tf;
                memcpy(&tf.matrix, matrix, 16 * sizeof(double));
                tf.time = -1;
                transformations.push_back(tf);
                double idM[16] = {1,0,0,0,
                                0,1,0,0,
                                0,0,1,0,
                                0,0,0,1};
                matrix = idM;

                transformation new_tf;
                new_tf.time = time;

                double x = elem->DoubleAttribute("axisX");
                double y = elem->DoubleAttribute("axisY");
                double z = elem->DoubleAttribute("axisZ");
                new_tf.points.push_back({x,y,z});

                transformations.push_back(new_tf);
            }
            else {
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
            parseGroup(elem, matrix, colors, transformations);
        }
        elem = elem->NextSiblingElement();
    }
    
    if(hasModels) {
        transformation tf;
        memcpy(&tf.matrix, matrix, 16 * sizeof(double));
        tf.time = -1;
        transformations.push_back(tf);

        struct group x;
        x.solids = solids;
        memcpy(&x.colors, colors, 3 * sizeof(int));
        x.transformations = transformations;
        int n = 0;
        for(size_t solid = 0; solid < solids.size(); solid++) {
            n += solids.at(solid).size();
        }
        x.numVertices = n;

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

    glPolygonMode(GL_FRONT_AND_BACK, filledSolids ? GL_FILL : GL_LINE);

    for (unsigned long group = 0; group < scene.size(); group++) {
        glPushMatrix();
        vector<transformation> & transforms = scene[group].transformations;
        for(int transform = transforms.size() - 1; transform >= 0; transform--) {
            if(transforms[transform].time > 0) {
                if(transforms[transform].points.size() > 1) {
                    size_t numPoints = transforms[transform].points.size();
                    double t = numPoints * (glutGet(GLUT_ELAPSED_TIME) / 1000.0) / transforms[transform].time; // this is the real global t
                    int index = floor(t);  // which segment
                    t = t - index; // where within the segment

                    // indices store the points
                    int indices[4]; 
                    indices[0] = (index + numPoints-1)%numPoints;	
                    indices[1] = (indices[0]+1)%numPoints;
                    indices[2] = (indices[1]+1)%numPoints;
                    indices[3] = (indices[2]+1)%numPoints;

                    // catmull-rom matrix
                    double m[4][4] = {	{-0.5f,  1.5f, -1.5f,  0.5f},
                                        { 1.0f, -2.5f,  2.0f, -0.5f},
                                        {-0.5f,  0.0f,  0.5f,  0.0f},
                                        { 0.0f,  1.0f,  0.0f,  0.0f}};

                    double pos[3];
                    double deriv[3];

                    for(int i = 0; i < 3; i++) {	 // x, y, z
                        double p[4] = {transforms[transform].points[indices[0]][i], transforms[transform].points[indices[1]][i],
                                        transforms[transform].points[indices[2]][i], transforms[transform].points[indices[3]][i]};
                        double a[4];

                        // Compute A = M * P
                        multMatrixVector(m, p, a);
                        
                        // Compute pos = T * A
                        pos[i] = pow(t,3.0) * a[0] + pow(t,2.0) * a[1] + t * a[2] + a[3];

                        // compute deriv = T' * A
                        deriv[i] = 3*pow(t,2.0) * a[0] + 2 * t * a[1] + a[2];
                    }
                    glTranslated(pos[0],pos[1],pos[2]);   
                    double x[3] = {deriv[0], deriv[1], deriv[2]};
                    normalize(x);
                    double z[3];
                    cross(x, transforms[transform].prev_y, z);
                    normalize(z);
                    double y[3];
                    cross(z,x,y);
                    normalize(y);
                    memcpy(transforms[transform].prev_y, y, 3 * sizeof(double));

                    double rotm[16];

                    buildRotMatrix(x,y,z,rotm);

                    glMultMatrixd(rotm);         
                }
                else {
                    double angle = 360.0 * (glutGet(GLUT_ELAPSED_TIME) / 1000.0) / transforms[transform].time;
                    while(angle > 360) angle -= 360;
                    glRotated(angle, transforms[transform].points[0][0],
                                    transforms[transform].points[0][1],
                                    transforms[transform].points[0][2]);
                }
            } else {
                glMultMatrixd(transforms[transform].matrix);
            }
        }

        glColor3ub(scene[group].colors[0],scene[group].colors[1],scene[group].colors[2]);
        
        if(VBOs) {
            glBindBuffer(GL_ARRAY_BUFFER, buffers.at(group));
            glVertexPointer(3, GL_DOUBLE, 0, 0);

            glDrawArrays(GL_TRIANGLES, 0, scene[group].numVertices);
        }
        else {
            std::vector<std::vector<Point>> & solids = scene[group].solids;
            for (unsigned long i = 0; i < solids.size(); i++) {
                std::vector<Point> & solid = solids[i];
                glBegin(GL_TRIANGLES);
                for (unsigned long j = 0; j < solid.size(); j++)
                    glVertex3d(solid[j].x(), solid[j].y(), solid[j].z());
                glEnd();
            }
        }

        glPopMatrix();
    }

    framerate();

    glutSwapBuffers();
}

#pragma region Controls

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
    if (move) {
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

        if(FPS) {
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
            
        }
        else {
            alpha += 2 * M_PI * deltax * sensitivity / 360;
            beta -= 2 * M_PI * deltay * sensitivity / 360;

            spherical2Cartesian();
        }
        glutPostRedisplay();
    }
}

#pragma endregion

void initVBOs() {
    vector<GLuint> vertices(scene.size());
    buffers = vertices;
    glGenBuffers(buffers.size(), buffers.data());

    for(size_t group = 0; group < scene.size(); group++) {
        vector<double> vertices;
        vector<vector<Point>> &solids = scene.at(group).solids;
        for(size_t solid = 0; solid < solids.size(); solid++) {
            for(size_t point = 0; point < solids.at(solid).size(); point++) {
                vertices.push_back(solids.at(solid).at(point).x());
                vertices.push_back(solids.at(solid).at(point).y());
                vertices.push_back(solids.at(solid).at(point).z());
            }
        }
        glBindBuffer(GL_ARRAY_BUFFER, buffers.at(group));
        glBufferData(GL_ARRAY_BUFFER, vertices.size() * sizeof(double), vertices.data(), GL_STATIC_DRAW);
    }
}

void menu(int option) {
    if(option == 1) {
        if (VBOs) {
            VBOs = false;
            glutChangeToMenuEntry(1,"Enable VBOs",1);   
        }
        else {
            VBOs = true;
            glutChangeToMenuEntry(1,"Disable VBOs",1);   
        }
    }
    else if(option == 2) {
        if (filledSolids) {
            filledSolids = false;
            glutChangeToMenuEntry(2,"Fill models",2); 
        }
        else {
            filledSolids = true;
            glutChangeToMenuEntry(2,"Line models",2); 
        }
    }
    glutPostRedisplay();
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

    vector<transformation> transformations;

    parseGroup(scene, idM, colors, transformations);

    //init GLUT and the Window
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
    glutInitWindowPosition(100,100);
    glutInitWindowSize(800,800);
    glutCreateWindow("Projeto CG");

    //Required callback registry
    glutDisplayFunc(renderScene);
    glutIdleFunc(renderScene);
    glutKeyboardFunc(keyboardFunc);
	glutReshapeFunc(changeSize);
    glutMouseFunc(mouseFunc);
    glutMotionFunc(motionFunc);

	glewInit();
	glEnableClientState(GL_VERTEX_ARRAY);

    initVBOs();

    //OpenGL settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    spherical2Cartesian();

    glutCreateMenu(menu);
    glutAddMenuEntry("Disable VBOs",1);
    glutAddMenuEntry("Fill models",2); 
    glutAttachMenu(GLUT_RIGHT_BUTTON);

    timebase = glutGet(GLUT_ELAPSED_TIME); 

    puts("Controls for 3rd person camera: \n\
WASD / Left mouse click + mouse movement - rotate\n\
Q - zoom in\n\
E - zoom out\n\
\n\
Controls for 1st person camera:\n\
WASD - move\n\
Left mouse click + mouse movement - look around\n\
\n\
V - switch between 1st and 3rd person modes\n\
Right mouse button - menu for toggling VBOs");

    //enter GLUT's main cycle
    glutMainLoop();

    return 0;
}