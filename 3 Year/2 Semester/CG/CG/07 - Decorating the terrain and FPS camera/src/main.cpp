#include<stdio.h>
#include<stdlib.h>

#define _USE_MATH_DEFINES
#include<math.h>
#include <vector>
#include <utility>

#include<IL/il.h>

#ifdef __APPLE__
#include<GLUT/glut.h>
#else
#include<GL/glew.h>
#include<GL/glut.h>
#endif

float camX = 00, camY = 30, camZ = 40;
int startX, startY, tracking = 0;

int alpha = 0, beta = 45, radius = 50;

unsigned int t;
int tw, th;
unsigned char * imageData;

GLuint* vertex;
double vertexCout;

int r = 50;
int ri = 35;
int rc = 15;

double hf(double x , double z);

std::vector<std::pair<double , double>> trees;

double h(int i , int j) {
	double x = imageData[i * tw + j];
	return x / 255 * 30;
}

double hf(double x , double z) {
	double x1 = floor(x);
	double z1 = floor(z);
	double x2 = x1 + 1;
	double z2 = z1 + 1;
	double fx = x - x1;
	double fz = z - z1;
	double h_x1_z = h(x1 , z1) * (1-fz) + h(x1 , z2) * fz;
	double h_x2_z = h(x2 , z1) * (1-fz) + h(x2 , z2) * fz;
	return h_x1_z * (1 - fx) + h_x2_z * fx;
}

void changeSize(int w , int h) {
	// Prevents a divide by zero , when Window is too short
	if (h == 0)
		h = 1;
	// Compute Window's aspect ratio
	float ratio = w * 1.0 / h;
	// Reset the coordinate system before modifying
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	// Set the viewport to be the entire Window
	glViewport(0 , 0 , w , h);
	// Set the correct perspective
	gluPerspective(45 , ratio , 1 , 1000);
	// Return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}

void drawTerrain() {
	glColor3ub(36 , 154 , 39);
	for (int strip = 0 ; strip < th - 1 ; strip++) {
		glBindBuffer(GL_ARRAY_BUFFER , vertex[strip]);
		glVertexPointer(3 , GL_DOUBLE , 0 , 0);
		glDrawArrays(GL_TRIANGLE_STRIP , 0 , vertexCout);
	}
	glColor3f(0.2f , 0.8f , 0.2f);
	glBegin(GL_TRIANGLES);
		glVertex3f(100.0f , 0 , - 100.0f);
		glVertex3f(- 100.0f , 0 , - 100.0f);
		glVertex3f(- 100.0f , 0 , 100.0f);

		glVertex3f(100.0f , 0 , - 100.0f);
		glVertex3f(- 100.0f , 0 , 100.0f);
		glVertex3f(100.0f , 0 , 100.0f);
	glEnd();
	glColor3ub(255 , 0 , 0);
	for (int i = 0 ; i < 16 ; i++) {
		glPushMatrix();
		glRotated(22.5 * i , 0 , 1 , 0);
		glTranslated(ri , 1 , 0);
		glutSolidTeapot(2);
		glPopMatrix();
	}
	glColor3ub(0 , 0 , 255);
	for (int i = 0 ; i < 16 ; i++) {
		glPushMatrix();
		glRotated(45.0 * i , 0 , 1 , 0);
		glTranslated(rc , 1 , 0);
		glutSolidTeapot(2);
		glPopMatrix();
	}
	glColor3ub(255 , 0 , 255);
	glutSolidTorus(1 , 3 , 10 , 10);
	for (unsigned int i = 0 ; i < trees.size() ; i++) {
		glPushMatrix();
		std::pair<double , double> posS = trees.at(i);
		double posX = posS.first;
		double posZ = posS.second;
		double posY = hf(posX + tw / 2 , posZ + th / 2);
		glTranslated(posX , posY , posZ);
		glRotated(- 90 , 1 , 0 , 0);
		glColor3ub(102 , 62 , 36);
		glutSolidCone(0.5 , 2 , 10 , 10);
		glTranslated(0 , 0 , 2);
		glutSolidCone(2 , 4 , 10 , 10);
		glPopMatrix();
	}
}

void renderScene(void) {
	glClearColor(0.0f , 0.0f , 0.0f , 0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glLoadIdentity();
	gluLookAt(camX , camY , camZ ,
			  0.0  , 0.0  , 0.0  ,
			  0.0f , 1.0f , 0.0f );
	glPolygonMode(GL_FRONT_AND_BACK , GL_LINE);
	drawTerrain();
	glutSwapBuffers();
}

void processKeys(unsigned char key , int xx , int yy) {}

void processMouseButtons(int button , int state , int xx , int yy) {
	if (state == GLUT_DOWN) {
		startX = xx;
		startY = yy;
		if (button == GLUT_LEFT_BUTTON)
			tracking = 1;
		else if (button == GLUT_RIGHT_BUTTON)
			tracking = 2;
		else
			tracking = 0;
	}
	else if (state == GLUT_UP) {
		if (tracking == 1) {
			alpha += (xx - startX);
			beta += (yy - startY);
		}
		else if (tracking == 2) {
			radius -= yy - startY;
			if (radius < 3)
				radius = 3.0;
		}
		tracking = 0;
	}
}

void processMouseMotion(int xx , int yy) {
	int deltaX;
	int deltaY;
	int alphaAux;
	int betaAux;
	int rAux;
	if (!tracking)
		return;
	deltaX = xx - startX;
	deltaY = yy - startY;
	if (tracking == 1) {
		alphaAux = alpha + deltaX;
		betaAux = beta + deltaY;
		if (betaAux > 85.0)
			betaAux = 85.0;
		else if (betaAux < - 85.0)
			betaAux = - 85.0;
		rAux = radius;
	}
	else if (tracking == 2) {
		alphaAux = alpha;
		betaAux = beta;
		rAux = radius - deltaY;
		if (rAux < 3)
			rAux = 3;
	}
	camX = rAux * sin(alphaAux * 3.14 / 180.0) * cos(betaAux * 3.14 / 180.0);
	camZ = rAux * cos(alphaAux * 3.14 / 180.0) * cos(betaAux * 3.14 / 180.0);
	camY = rAux * sin(betaAux  * 3.14 / 180.0);
}

void init() {
	// Load the hight map "terrain.jpg"
	ilGenImages(1 , &t);
	ilBindImage(t);
	ilLoadImage((ILstring)"terrain.jpg");
	ilConvertImage(IL_LUMINANCE , IL_UNSIGNED_BYTE);
	tw = ilGetInteger(IL_IMAGE_WIDTH);
	th = ilGetInteger(IL_IMAGE_HEIGHT);
	imageData = ilGetData();
	// Build the Vertex Arrays
	vertex = (GLuint *)calloc(th-1 , sizeof(GLuint));
	glGenBuffers(th-1 , vertex);
	int halfW = tw / 2;
	int halfH = th / 2;
	vertexCout = 2 * tw;
	for (int n_strip = 0 ; n_strip < th - 1 ; n_strip++) {
		std::vector<double> strip;
		for (int j = 0 ; j < tw ; j++) {
			strip.push_back(n_strip - halfH);
			strip.push_back(h(n_strip , j));
			strip.push_back(j - halfW);

			strip.push_back(n_strip + 1 - halfH);
			strip.push_back(h(n_strip + 1 , j));
			strip.push_back(j - halfW);
		}
		glBindBuffer(GL_ARRAY_BUFFER , vertex[n_strip]);
		glBufferData(GL_ARRAY_BUFFER , strip.size() * sizeof(double) , strip.data() , GL_STATIC_DRAW);
	}
	for (int tree = 0 ; tree < 300 ; tree++) {
		double posX = 0;
		double posZ = 0;
		while (pow(posX , 2) + pow(posZ , 2) < pow(r , 2)) {
			posX = (rand() % (r * 4)) - (r * 2);
			posZ = (rand() % (r * 4)) - (r * 2);
		}
		trees.push_back(std::make_pair(posX , posZ));
	}
	//OpenGL settings
	glEnable(GL_DEPTH_TEST);
}

int main(int argc , char **argv) {
	// Init GLUT and the Window
	glutInit(&argc , argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100 , 100);
	glutInitWindowSize(320 , 320);
	glutCreateWindow("Gaia");
	// Required callback registry
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);
	// Callback registration for keyboard processing
	glutKeyboardFunc(processKeys);
	glutMouseFunc(processMouseButtons);
	glutMotionFunc(processMouseMotion);
	glewInit();
	glEnableClientState(GL_VERTEX_ARRAY);
	ilInit();
	init();
	// Enter GLUT's main cycle
	glutMainLoop();
	return 0;
}
