#include <stdio.h>
#include <stdlib.h>

#ifdef __APPLE__ 
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#endif

#define _USE_MATH_DEFINES
#include <math.h>
#include <vector>

using namespace std;

GLuint vertex;
GLuint vertexCount;
vector<double> vertexB;
bool aux = false;
double slices = 60;
double timebase;
double frames;
double pos = M_PI_4;
double oldx = 0, oldy = 0;
double aux1 = M_PI;

struct Polar {
	double radius;
	double alpha;
	double beta;
};

Polar cam = { sqrt(75), pos, pos };

double polarX(Polar polar) {
	double res = polar.radius * cos(polar.beta) * sin(polar.alpha);
	return res;
}

double polarY(Polar polar) {
	double res = polar.radius * sin(polar.beta);
	return res;
}

double polarZ(Polar polar) {
	double res = polar.radius * cos(polar.beta) * cos(polar.alpha);
	return res;
}


void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window with zero width).
	if(h == 0)
		h = 1;

	// compute window's aspect ratio 
	float ratio = w * 1.0 / h;

	// Set the projection matrix as current
	glMatrixMode(GL_PROJECTION);
	// Load Identity Matrix
	glLoadIdentity();
	
	// Set the viewport to be the entire window
    glViewport(0, 0, w, h);

	// Set perspective
	gluPerspective(45.0f ,ratio, 1.0f ,1000.0f);

	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}

void framerate() {
	char title[50];
	frames++;
	double time = glutGet(GLUT_ELAPSED_TIME);
	if (time - timebase > 1000) {
		double fps = frames * 1000.0 / (time - timebase);
		timebase = time;
		frames = 0;
		sprintf(title, "CG@DI-UM | %lf FPS", fps);
		glutSetWindowTitle(title);
	}
}


/*-----------------------------------------------------------------------------------
	Draw Cylinder

		parameters: radius, height, sides

-----------------------------------------------------------------------------------*/


void cylinder(float radius, float height, int sides) {
	vertexB.clear();
	double h = height / 2;
	double stepSlice = M_PI * 2 / slices;

	Polar baseP = { h,0,- M_PI / 2 };
	Polar topP = { h,0,M_PI / 2 };
	// BASE
	glVertex3d(polarX(baseP), polarY(baseP), polarZ(baseP));
	for (unsigned int slice = slices; slice > 0; slice--) {
		Polar p = { sqrt(pow(h, 2) + pow(radius,2)) , stepSlice * slice ,  -atan(h / radius) };
		Polar pp = { sqrt(pow(h, 2) + pow(radius,2)) , stepSlice * (slice + 1) ,  -atan(h / radius) };

		vertexB.push_back(polarX(baseP));
		vertexB.push_back(polarY(baseP));
		vertexB.push_back(polarZ(baseP));

		vertexB.push_back(polarX(pp));
		vertexB.push_back(polarY(pp));
		vertexB.push_back(polarZ(pp));

		vertexB.push_back(polarX(p));
		vertexB.push_back(polarY(p));
		vertexB.push_back(polarZ(p));
	}

	// TOP
	glVertex3d(polarX(topP), polarY(topP), polarZ(topP));
	for (unsigned int slice = 0; slice < slices; slice++) {
		Polar p = { sqrt(pow(h, 2) + pow(radius,2)) , stepSlice * slice ,  atan(h / radius) };
		Polar pp = { sqrt(pow(h, 2) + pow(radius,2)) , stepSlice * (slice + 1) ,  atan(h / radius) };

		vertexB.push_back(polarX(topP));
		vertexB.push_back(polarY(topP));
		vertexB.push_back(polarZ(topP));

		vertexB.push_back(polarX(p));
		vertexB.push_back(polarY(p));
		vertexB.push_back(polarZ(p));

		vertexB.push_back(polarX(pp));
		vertexB.push_back(polarY(pp));
		vertexB.push_back(polarZ(pp));
	}
	// SIDE
	for (unsigned int slice = 0; slice < slices; slice++) {
		Polar pb = { sqrt(pow(h, 2) + pow(radius,2)) , stepSlice * slice ,  -atan(h / radius) };
		Polar pt = { sqrt(pow(h, 2) + pow(radius,2)) , stepSlice * slice ,  atan(h / radius) };

		Polar ppb = { sqrt(pow(h, 2) + pow(radius,2)) , stepSlice * (slice + 1) ,  -atan(h / radius) };
		Polar ppt = { sqrt(pow(h, 2) + pow(radius,2)) , stepSlice * (slice + 1) ,  atan(h / radius) };

		vertexB.push_back(polarX(pt));
		vertexB.push_back(polarY(pt));
		vertexB.push_back(polarZ(pt));

		vertexB.push_back(polarX(pb));
		vertexB.push_back(polarY(pb));
		vertexB.push_back(polarZ(pb));

		vertexB.push_back(polarX(ppb));
		vertexB.push_back(polarY(ppb));
		vertexB.push_back(polarZ(ppb));

		vertexB.push_back(polarX(ppb));
		vertexB.push_back(polarY(ppb));
		vertexB.push_back(polarZ(ppb));

		vertexB.push_back(polarX(ppt));
		vertexB.push_back(polarY(ppt));
		vertexB.push_back(polarZ(ppt));

		vertexB.push_back(polarX(pt));
		vertexB.push_back(polarY(pt));
		vertexB.push_back(polarZ(pt));
	}
	vertexCount = vertexB.size() / 3;

	glBindBuffer(GL_ARRAY_BUFFER, vertex);
	glBufferData(GL_ARRAY_BUFFER, vertexB.size() * sizeof(double), vertexB.data(), GL_STATIC_DRAW);
}


void renderScene(void) {

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
	gluLookAt(polarX(cam), polarY(cam), polarZ(cam),
		0.0, 0.0, 0.0,
		0.0f, 1.0f, 0.0f);

	if (aux) {
		glColor3ub(255, 0, 255);
		glBindBuffer(GL_ARRAY_BUFFER, vertex);
		glVertexPointer(3, GL_DOUBLE, 0, 0);
		glDrawArrays(GL_TRIANGLES, 0, vertexCount);
	}
	else {
		glColor3ub(255, 150, 0);
		glBegin(GL_TRIANGLES);
		for (size_t i = 0; i < vertexB.size(); i += 3) {
			glVertex3d(vertexB.at(i), vertexB.at(i + 1), vertexB.at(i + 2));
		}
		glEnd();
	}
	framerate();

	// End of frame
	glutSwapBuffers();
}



void keyboardFunc(unsigned char key, int x, int y) {
	switch (key) {
	case 'a':
		cam.alpha -= M_PI / 16;
		break;
	case 'd':
		cam.alpha += M_PI / 16;
		break;
	case 's':
		cam.beta -= M_PI / 16;
		break;
	case 'w':
		cam.beta += M_PI / 16;
		break;
	case '+':
		slices *= 2;
		cylinder(1, 2, slices);
		printf("slices = %p\n", slices);
		break;
	case '-':
		slices /= 2;
		cylinder(1, 2, slices);
		printf("slices = %p\n", slices);
		break;
	}
	if (cam.alpha < 0) cam.alpha += aux1 * 2;
	else if (cam.alpha > aux1 * 2) cam.alpha -= aux1 * 2;
	if (cam.beta < -aux1) cam.beta += aux1 * 2;
	else if (cam.beta > aux1) cam.beta -= aux1 * 2;
	glutPostRedisplay();
}

void motionFunc(int x, int y) {
	int deltax = x - oldx;
	int deltay = y - oldy;

	cam.alpha += 2 * aux1 * deltax / 360;
	cam.beta += 2 * aux1 * deltay / 360;

	oldx = x;
	oldy = y;

	glutPostRedisplay();
}



void printInfo() {

	printf("Vendor: %s\n", glGetString(GL_VENDOR));
	printf("Renderer: %s\n", glGetString(GL_RENDERER));
	printf("Version: %s\n", glGetString(GL_VERSION));

	printf("\nUse Arrows to move the camera up/down and left/right\n");
	printf("Page Up and Page Down control the distance from the camera to the origin");
}


int main(int argc, char **argv) {

// init GLUT and the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(800,800);
	glutCreateWindow("GAIA-Rui");
		
// Required callback registry 
	glutDisplayFunc(renderScene);
	glutReshapeFunc(changeSize);
	
// Callback registration for keyboard processing
	glutKeyboardFunc(keyboardFunc);
	glutMotionFunc(motionFunc);

	// init GLEW
#ifndef __APPLE__
	glewInit();
#endif

	glEnableClientState(GL_VERTEX_ARRAY);

	glGenBuffers(1, &vertex);
	cylinder(1, 2, slices);


//  OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT, GL_LINE);


	printInfo();

// enter GLUT's main cycle
	timebase = glutGet(GLUT_ELAPSED_TIME);
	glutMainLoop();
	
	return 1;
}
