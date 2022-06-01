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

GLuint buff[2];
int vertex_count;
vector<double> vertexB;
vector<double> normalB;
bool vbo = true;
int slices = 50;

int timebase;
double frames;

int oldx = 0, oldy = 0;
bool firstMouse = true;

struct Polar {
    double radius;
    double alpha;
    double beta;
};

Polar camPos = { sqrt(75), M_PI_4, M_PI_4 };

double polarX(Polar polar) {
    return polar.radius * cos(polar.beta) * sin(polar.alpha);
}

double polarY(Polar polar) {
    return polar.radius * sin(polar.beta);
}

double polarZ(Polar polar) {
    return polar.radius * cos(polar.beta) * cos(polar.alpha);
}



void changeSize(int w , int h) {
    if(h == 0)
        h = 1;
    float ratio = w * 1.0 / h;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glMatrixMode(GL_MODELVIEW);
}

void framerate() {
    char title[50];
    frames++;
    double time = glutGet(GLUT_ELAPSED_TIME);
    if(time - timebase > 1000) {
        double fps = frames * 1000.0 / (time - timebase);
        timebase = time;
        frames = 0;
        sprintf(title, "GAIA | %lf FPS", fps);
        glutSetWindowTitle(title);
    }
}


void genCylinder(double radius, double height, unsigned int slices) {
    vertexB.clear();
    normalB.clear();

	double hh = height / 2;
	double sliceStep = M_PI * 2 / slices;

	Polar baseP = { hh, 0, - M_PI / 2 };
	Polar topP = { hh, 0, M_PI / 2 };

	// BASE
	for(unsigned int slice = slices; slice > 0; slice--) {
		Polar p = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * slice ,  - atan(hh / radius)};
        Polar pp = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * (slice + 1) ,  - atan(hh / radius)};

        vertexB.push_back(polarX(baseP));
        vertexB.push_back(polarY(baseP));
        vertexB.push_back(polarZ(baseP));

        normalB.push_back(0);normalB.push_back(-1);normalB.push_back(1);

        vertexB.push_back(polarX(pp));
        vertexB.push_back(polarY(pp));
        vertexB.push_back(polarZ(pp));

        normalB.push_back(0);normalB.push_back(-1);normalB.push_back(1);

        vertexB.push_back(polarX(p));
        vertexB.push_back(polarY(p));
        vertexB.push_back(polarZ(p));

        normalB.push_back(0);normalB.push_back(-1);normalB.push_back(1);
	}

	// TOP
	for(unsigned int slice = 0; slice < slices; slice++) {
		Polar p = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * slice ,  atan(hh / radius)};
        Polar pp = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * (slice + 1) ,  atan(hh / radius)};

        vertexB.push_back(polarX(topP));
        vertexB.push_back(polarY(topP));
        vertexB.push_back(polarZ(topP));

        normalB.push_back(0);normalB.push_back(1);normalB.push_back(1);

        vertexB.push_back(polarX(p));
        vertexB.push_back(polarY(p));
        vertexB.push_back(polarZ(p));
        
        normalB.push_back(0);normalB.push_back(1);normalB.push_back(1);

        vertexB.push_back(polarX(pp));
        vertexB.push_back(polarY(pp));
        vertexB.push_back(polarZ(pp));

        normalB.push_back(0);normalB.push_back(1);normalB.push_back(1);
	}

	// SIDE
	for(unsigned int slice = 0; slice < slices; slice++) {
		Polar pb = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * slice ,  - atan(hh / radius)};
		Polar pt = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * slice ,  atan(hh / radius)};
        Polar ppb = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * (slice + 1) ,  - atan(hh / radius)};
		Polar ppt = { sqrt(pow(hh, 2) + pow(radius,2)) , sliceStep * (slice + 1) ,  atan(hh / radius)};
        
        vertexB.push_back(polarX(pt));
        vertexB.push_back(polarY(pt));
        vertexB.push_back(polarZ(pt));

        normalB.push_back(sin(pt.alpha));normalB.push_back(0);normalB.push_back(cos(pt.alpha));

        vertexB.push_back(polarX(pb));
        vertexB.push_back(polarY(pb));
        vertexB.push_back(polarZ(pb));

        normalB.push_back(sin(pb.alpha));normalB.push_back(0);normalB.push_back(cos(pb.alpha));

        vertexB.push_back(polarX(ppb));
        vertexB.push_back(polarY(ppb));
        vertexB.push_back(polarZ(ppb));

        normalB.push_back(sin(ppb.alpha));normalB.push_back(0);normalB.push_back(cos(ppb.alpha));

        vertexB.push_back(polarX(ppb));
        vertexB.push_back(polarY(ppb));
        vertexB.push_back(polarZ(ppb));

        normalB.push_back(sin(ppb.alpha));normalB.push_back(0);normalB.push_back(cos(ppb.alpha));

        vertexB.push_back(polarX(ppt));
        vertexB.push_back(polarY(ppt));
        vertexB.push_back(polarZ(ppt));

        normalB.push_back(sin(ppt.alpha));normalB.push_back(0);normalB.push_back(cos(ppt.alpha));

        vertexB.push_back(polarX(pt));
        vertexB.push_back(polarY(pt));
        vertexB.push_back(polarZ(pt));

        normalB.push_back(sin(pt.alpha));normalB.push_back(0);normalB.push_back(cos(pt.alpha));
	}
    vertex_count = vertexB.size() / 3;
    glBindBuffer(GL_ARRAY_BUFFER, buff[0]);
    glBufferData(GL_ARRAY_BUFFER, vertexB.size() * sizeof(double), vertexB.data(), GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, buff[1]);
    glBufferData(GL_ARRAY_BUFFER, normalB.size() * sizeof(double), normalB.data(), GL_STATIC_DRAW);
}


void renderScene(void) {
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glLoadIdentity();
	gluLookAt(polarX(camPos), polarY(camPos), polarZ(camPos), 
		      0.0,0.0,0.0,
			  0.0f,camPos.beta > M_PI_2 ? -1.0f : 1.0f,0.0f);

    float pos[4] = {3.0,3.0,3.0,0.0};
    glLightfv(GL_LIGHT0, GL_POSITION, pos);
    float dark[4] = {0.2,0.2,0.2,1.0};
    float white[4] = {0.8,0.8,0.8,1.0};
    float red[4] = {0.8,0.2,0.2,1.0};
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, white);
    glMaterialfv(GL_FRONT, GL_SPECULAR, white);
    glMaterialf(GL_FRONT, GL_SHININESS, 64);
    if(vbo) {
        glBindBuffer(GL_ARRAY_BUFFER, buff[0]);
        glVertexPointer(3, GL_DOUBLE, 0, 0);
        glBindBuffer(GL_ARRAY_BUFFER, buff[1]);
        glNormalPointer(GL_DOUBLE, 0, 0);
        glDrawArrays(GL_TRIANGLES, 0, vertex_count);

    }
    else 
        glBegin(GL_TRIANGLES);
        for(size_t i = 0; i < vertexB.size(); i += 3) {
            glVertex3d(vertexB.at(i), vertexB.at(i+1), vertexB.at(i+2));
        }
    glEnd();

    framerate();

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
        case 'v':
            vbo = vbo ? false : true;
            break;
        case '+':
            slices *= 2;
            genCylinder(1, 2, slices);
            printf("slices = %d\n", slices);
            break;
        case '-':
            slices /= 2;
            genCylinder(1, 2, slices);
            printf("slices = %d\n", slices);
            break;
	}
	if (camPos.alpha < 0) camPos.alpha += M_PI * 2;
	else if (camPos.alpha > M_PI * 2) camPos.alpha -= M_PI * 2;
    if (camPos.beta < - M_PI_2) camPos.beta += M_PI * 2;
    else if (camPos.beta > (3 * M_PI_2)) camPos.beta -= M_PI * 2;
	glutPostRedisplay();
}


void mouseFunc(int button, int state, int x, int y) {
    if(button == GLUT_LEFT_BUTTON) {
        if (state == GLUT_DOWN)
            firstMouse = true;
    }
}


void motionFunc(int x, int y) {
    if(firstMouse) {
        oldx = x;
        oldy = y;
        firstMouse = false;
        return;
    }
    int deltax = x - oldx;
    int deltay = oldy - y;

    camPos.alpha += 2 * M_PI * deltax * 0.1 / 360;
    camPos.beta -= 2 * M_PI * deltay * 0.1 / 360;

    if (camPos.beta < - M_PI_2) camPos.beta += M_PI * 2;
    else if (camPos.beta > (3 * M_PI_2)) camPos.beta -= M_PI * 2;

    oldx = x;
    oldy = y;

    glutPostRedisplay();
}


int main(int argc, char **argv) {

// init GLUT and the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(800,800);
	glutCreateWindow("GAIA");
		
// Required callback registry 
	glutDisplayFunc(renderScene);
    glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);

    glewInit();

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);

    glGenBuffers(2, buff);
    genCylinder(1, 2, slices);

// put here the registration of the keyboard callbacks

	glutKeyboardFunc(keyboardFunc);
    glutMotionFunc(motionFunc);
    glutMouseFunc(mouseFunc);

//  OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    GLfloat dark[4] = {0.2,0.2,0.2,1.0};
    GLfloat white[4] = {1.0,1.0,1.0,1.0};

    glLightfv(GL_LIGHT0, GL_AMBIENT, dark);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, white);
    glLightfv(GL_LIGHT0, GL_SPECULAR, white);
	

    timebase = glutGet(GLUT_ELAPSED_TIME); 

	glutMainLoop();
	
	return 1;
}