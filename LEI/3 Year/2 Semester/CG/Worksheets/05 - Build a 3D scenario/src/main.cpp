#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#define _USE_MATH_DEFINES
#include <math.h>



float alpha = 0.0f, beta = 0.5f, radius = 100.0f;
float camX, camY, camZ;



void spherical2Cartesian() {

    camX = radius * cos(beta) * sin(alpha);
    camY = radius * sin(beta);
    camZ = radius * cos(beta) * cos(alpha);
}



int r = 50;
int ri = 35;
int rc = 15;



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



void renderScene(void) {

    // clear buffers
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // set the camera
    glLoadIdentity();
    gluLookAt(camX, camY, camZ,
        0.0, 0.0, 0.0,
        0.0f, 1.0f, 0.0f);

    glColor3f(0.2f, 0.8f, 0.2f);
    glBegin(GL_TRIANGLES);
    glVertex3f(100.0f, 0, -100.0f);
    glVertex3f(-100.0f, 0, -100.0f);
    glVertex3f(-100.0f, 0, 100.0f);

    glVertex3f(100.0f, 0, -100.0f);
    glVertex3f(-100.0f, 0, 100.0f);
    glVertex3f(100.0f, 0, 100.0f);
    glEnd();
    // End of frame


    // put code to draw scene in here

    glColor3ub(255, 0, 0);
    for (int i = 0; i < 16; i++) {
        glPushMatrix();
        glRotated(22.5 * i, 0, 1, 0);
        glTranslated(ri, 1, 0);
        glutSolidTeapot(2);
        glPopMatrix();
    }

    glColor3ub(0, 0, 255);
    for (int i = 0; i < 8; i++) {
        glPushMatrix();
        glRotated(45.0 * i, 0, 1, 0);
        glTranslated(rc, 1, 0);
        glutSolidTeapot(2);
        glPopMatrix();
    }

    glColor3ub(255, 0, 255);
    glutSolidTorus(1, 2, 10, 10);

    for (int i = 0; i < 300; i++) {
        glPushMatrix();
        double posx = 0, posz = 0;
        while (pow(posx, 2) + pow(posz, 2) < pow(r, 2)) {
            posx = (rand() % (r * 4)) - (r * 2);
            posz = (rand() % (r * 4)) - (r * 2);
        }
        glTranslated(posx, 0, posz);
        glRotated(-90, 1, 0, 0);
        glColor3ub(102, 62, 36);
        glutSolidCone(0.5, 2, 10, 10);
        glTranslated(0, 0, 2);
        glColor3ub(0, 255, 0);
        glutSolidCone(2, 4, 10, 10);
        glPopMatrix();
    }

    glutSwapBuffers();
}



void processKeys(unsigned char c, int xx, int yy) {

    // put code to process regular keys in here
}



void processSpecialKeys(int key, int xx, int yy) {

    switch (key) {

    case GLUT_KEY_RIGHT:
        alpha -= 0.1; break;

    case GLUT_KEY_LEFT:
        alpha += 0.1; break;

    case GLUT_KEY_UP:
        beta += 0.1f;
        if (beta > 1.5f)
            beta = 1.5f;
        break;

    case GLUT_KEY_DOWN:
        beta -= 0.1f;
        if (beta < -1.5f)
            beta = -1.5f;
        break;

    case GLUT_KEY_PAGE_DOWN: radius -= 1.0f;
        if (radius < 1.0f)
            radius = 1.0f;
        break;

    case GLUT_KEY_PAGE_UP: radius += 1.0f; break;
    }
    spherical2Cartesian();
    glutPostRedisplay();

}



void printInfo() {

    printf("Vendor: %s\n", glGetString(GL_VENDOR));
    printf("Renderer: %s\n", glGetString(GL_RENDERER));
    printf("Version: %s\n", glGetString(GL_VERSION));

    printf("\nUse Arrows to move the camera up/down and left/right\n");
    printf("Home and End control the distance from the camera to the origin");
}



int main(int argc, char** argv) {

    srand(time(NULL));

    // init GLUT and the window
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(800, 800);
    glutCreateWindow("GAIA-Rui");

    // Required callback registry 
    glutDisplayFunc(renderScene);
    glutReshapeFunc(changeSize);

    // Callback registration for keyboard processing
    glutKeyboardFunc(processKeys);
    glutSpecialFunc(processSpecialKeys);

    //  OpenGL settings
    glEnable(GL_DEPTH_TEST);
    //glEnable(GL_CULL_FACE);

    spherical2Cartesian();

    printInfo();

    // enter GLUT's main cycle
    glutMainLoop();

    return 1;
}