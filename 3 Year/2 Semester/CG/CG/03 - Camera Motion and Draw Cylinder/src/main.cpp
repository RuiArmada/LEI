#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h> 
#endif

#define _USE_MATH_DEFINES
#include <math.h>

struct Polar {
	float radius;
	float alpha;
	float beta;
};

Polar camPos = { sqrt(75), M_PI_4, M_PI_4 };

float polarX(Polar polar) { 
	return polar.radius * cos(polar.beta) * sin(polar.alpha); 
}

float polarY(Polar polar) { 
	return polar.radius * sin(polar.beta);
}

float polarZ(Polar polar) { 
	return polar.radius * cos(polar.beta) * cos(polar.alpha); 
}


void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window with zero width).
	if (h == 0)
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
	gluPerspective(45.0f, ratio, 1.0f, 1000.0f);

	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}

void drawCylinder(double radius, double height, int slices) {
	double hh = height / 2;
	double sliceStep = M_PI * 2 / slices;

	Polar baseP = { hh, 0, -M_PI / 2 };
	Polar topP = { hh, 0, M_PI / 2 };

	// BASE
	glBegin(GL_TRIANGLE_FAN);
	glVertex3f(polarX(baseP), polarY(baseP), polarZ(baseP));

	for (int slice = slices; slice >= 0; slice--) {

		Polar p = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * slice ,  -atan(hh / radius) };
		glColor3f(1.0f, 1.0f, 0.0f);
		glVertex3f(polarX(p), polarY(p), polarZ(p));

	}
	glEnd();


	// TOP
	glBegin(GL_TRIANGLE_FAN);
	glVertex3f(polarX(topP), polarY(topP), polarZ(topP));
	for (int slice = 0; slice <= slices; slice++) {

		Polar p = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * slice ,  atan(hh / radius) };
		glColor3f(1.0f, 1.0f, 0.0f);
		glVertex3f(polarX(p), polarY(p), polarZ(p));

	}
	glEnd();

	// SIDE
	glBegin(GL_TRIANGLE_STRIP);
	for (int slice = 0; slice <= slices; slice++) {

		Polar pb = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * slice ,  -atan(hh / radius) };
		Polar pt = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * slice ,  atan(hh / radius) };
		glColor3f(1.0f, 0.0f, 0.0f);
		glVertex3f(polarX(pt), polarY(pt), polarZ(pt));
		glVertex3f(polarX(pb), polarY(pb), polarZ(pb));

	}
	glEnd();
}

void renderScene(void) {

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
	gluLookAt(polarX(camPos), polarY(camPos), polarZ(camPos),
		0.0, 0.0, 0.0,
		0.0f, 1.0f, 0.0f);

	// put the geometric transformations here


	// put drawing instructions here

	drawCylinder(1, 2, 20);

	// End of frame
	glutSwapBuffers();
}



// write function to process keyboard events


void keyboardFunc(unsigned char key, int x, int y) {
	switch (key) {
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
	}
	if (camPos.alpha < 0) camPos.alpha += M_PI * 2;

	else if (camPos.alpha > M_PI * 2) camPos.alpha -= M_PI * 2;

	if (camPos.beta < -M_PI) camPos.beta += M_PI * 2;

	else if (camPos.beta > M_PI) camPos.beta -= M_PI * 2;

	glutPostRedisplay();
}



int main(int argc, char** argv) {

	// init GLUT and the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(800, 800);
	glutCreateWindow("GAIA-Rui");

	// Required callback registry 
	glutDisplayFunc(renderScene);
	glutReshapeFunc(changeSize);


	// put here the registration of the keyboard callbacks

	glutKeyboardFunc(keyboardFunc);

	//  OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	// enter GLUT's main cycle
	glutMainLoop();

	return 1;
}