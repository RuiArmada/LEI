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

#include "tinyxml2.h"
#include "point.hpp"

using namespace tinyxml2;

XMLDocument doc;

std::vector<std::vector<Point>> solids;

struct Polar {
	double radius;
	double alpha;
	double beta;
};

Polar camPos = { sqrt(75), M_PI_4, M_PI_4 }; 

double polarX(Polar polar) { return polar.radius * cos(polar.beta) * sin(polar.alpha); }
double polarY(Polar polar) { return polar.radius * sin(polar.beta); }
double polarZ(Polar polar) { return polar.radius * cos(polar.beta) * cos(polar.alpha); }

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

void renderScene(){ //this function reads and draws the XML scene
    //buffer reset
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glLoadIdentity();
	gluLookAt(polarX(camPos), polarY(camPos), polarZ(camPos), 
		      0.0,0.0,0.0,
			  0.0f,camPos.beta > M_PI_2 ? -1.0f : 1.0f,0.0f);

    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

    puts("Rendering...");
    for (unsigned long i = 0; i < solids.size(); i++) {
        std::vector<Point> solid = solids[i];
        glBegin(GL_TRIANGLES);
            for (unsigned long j = 0; j < solid.size(); j++) {
                glColor3ub(i % 3 == 0 ? 255 : 0, i % 3 == 1 ? 255 : 0, i % 3 == 2 ? 255 : 0);
                glVertex3d(solid[j].x(), solid[j].y(), solid[j].z());
            }
        glEnd();
    }
    puts("Render complete.");

    glutSwapBuffers();
}

void keyboardFunc(unsigned char key, int x, int y) {
	switch(key) {
		case 'a':
			camPos.alpha -= M_PI / 16;
			break;
		case 'd':
			camPos.alpha += M_PI / 16;
			break;
		case 's':
			camPos.beta -= M_PI / 16;
			break;
		case 'w':
			camPos.beta += M_PI / 16;
			break;
        case 'q':
			if (camPos.radius > 1) camPos.radius -= 1;
			break;
		case 'e':
		    camPos.radius += 1;
			break;
	}
	if (camPos.alpha < 0) camPos.alpha += M_PI * 2;
	else if (camPos.alpha > M_PI * 2) camPos.alpha -= M_PI * 2;
	if (camPos.beta < - M_PI_2) camPos.beta += M_PI * 2;
	else if (camPos.beta > (3 * M_PI_2)) camPos.beta -= M_PI * 2;
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
        puts("No <scene> found.");
        return 1;
    }

    XMLElement* model = scene->FirstChildElement();
    while(model) {
        if(!strcmp(model->Name(), "model"))
            solids.push_back(vectorize(model->Attribute("file")));
        model = model->NextSiblingElement();
    }

    //init GLUT and the Window
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
    glutInitWindowPosition(100,100);
    glutInitWindowSize(800,800);
    glutCreateWindow("GAIA");

    //Required callback registry
    glutDisplayFunc(renderScene);
    glutKeyboardFunc(keyboardFunc);
	glutReshapeFunc(changeSize);

    //OpenGL settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    //enter GLUT's main cycle
    glutMainLoop();

    return 0;
}