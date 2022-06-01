#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include<stdio.h>

#include <math.h>

// Distance to Target
float dist = 700;
// Distance Between the Eyes
float disp = 30;
// Angle
float angle = 0;
// Colors
float cBlack[] = {0 , 0 , 0 , 0};
float cWhite[] = {1 , 1 , 1 , 1};
float cRed[] = {0.4 , 0 , 0};
float cGreen[] = {0 , 0.4 , 0.4};
float cRedish[] = {0.2 , 0 , 0};
float cGreenish[] = {0 , 0.2 , 0.2};

void cross(float *a, float *b, float *res) {
    res[0] = a[1] * b[2] - a[2] * b[1];
    res[1] = a[2] * b[0] - a[0] * b[2];
    res[2] = a[0] * b[1] - a[1] * b[0];
}

void normalize(float *a) {
    float l = sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]);
    a[0] = a[0]/l;
    a[1] = a[1]/l;
    a[2] = a[2]/l;
}

void changeSize(int w, int h) {
    if(h == 0)
        h = 1;
    float ratio = 1.0 * w/h;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0 , 0 , w , h);
    gluPerspective(45 , ratio , 400 , 10000);
    glMatrixMode(GL_MODELVIEW);
}

void renderScene(void) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glLoadIdentity();
	
    float lpos[] = {1 , 1 , 0 , 0};
	float posCam[] = {0.0f , 0.0f , dist};
	float dir[] = {0 , 0 , -1.f};
	float up[] = {0 , 1.f , 0};
	float r[3] = {1.f , 0.f , 0.f};
    float posr[] = {posCam[0] + (disp/2.f) * r[0] , posCam[1] + (disp/2.f) * r[1] , posCam[2] + (disp/2.f) * r[2]};
	float posl[] = {posCam[0] - (disp/2.f) * r[0] , posCam[1] - (disp/2.f) * r[1] , posCam[2] - (disp/2.f) * r[2]};
    float teapot_size = 150;

    glPushMatrix();
    glPushMatrix();
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glColorMask(GL_TRUE , GL_FALSE , GL_FALSE , GL_TRUE);
    gluLookAt(posl[0] , posl[1] , posl[2] , 0.0f , 0.0f , 0.0f , 0.0f , 1.0f , 0.0f);
    glRotatef(angle , 0 , 1 , 0);
    glutWireTeapot(teapot_size);

    glPopMatrix();
    glPushMatrix();
    glClear(GL_DEPTH_BUFFER_BIT);
    glColorMask(GL_FALSE , GL_TRUE , GL_TRUE , GL_TRUE);
    gluLookAt(posr[0] , posr[1] , posr[2] , 0.0f , 0.0f , 0.0f , 0.0f , 1.0f , 0.0f);
    glRotatef(angle , 0 , 1 , 0);
    glutWireTeapot(teapot_size);

    glColorMask(GL_TRUE , GL_TRUE , GL_TRUE , GL_TRUE);
    glutSwapBuffers();
    glPopMatrix();
    glPopMatrix();

    angle += 0.1;
}

void processNormalKeys(unsigned char key, int x, int y) {
    char s[20];
    if(key == 27)
        exit(0);
    switch(key) {
        
        case 'a' : dist *= 0.9;
                   break;
        
        case 's' : dist *= 1.1;
                   break;

    }
    sprintf(s , "%f" , dist);
    glutSetWindowTitle(s);
}

int main(int argc, char **argv) {
	glutInit(&argc , argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100 , 100);
	glutInitWindowSize(320 , 320);
	glutCreateWindow("Gaia");
	
	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutKeyboardFunc(processNormalKeys);
	
	glEnable(GL_DEPTH_TEST);

	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_COLOR_MATERIAL);
	glColorMaterial(GL_FRONT_AND_BACK , GL_AMBIENT_AND_DIFFUSE);

	glClearColor(0.0f , 0.0f , 0.0f , 0.0f);

	glutMainLoop();

	return 0;
}