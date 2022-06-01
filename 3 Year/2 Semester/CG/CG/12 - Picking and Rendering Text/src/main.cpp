#include <iostream>
#include <math.h>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>

#ifdef __APPLE__
    #include <GLUT/glut.h>
#else 
    #include <GL/glut.h>
#endif 

float camX = 00, camY = 30, camZ = 40;
int startX, startY, tracking = 0;

float alpha = 0, beta = 35, r = 10;

int w, h, timebase = 0, frame = 0;
char s[30];

std::string text = "Nothing Selected\n";

float orange[4] = {0.8f, 0.4f, 0.4f, 1.0f};
float green[4] = {0.4f, 0.8f, 0.4f, 1.0f};
float black[4] = {0.0f, 0.0f, 0.0f, 0.0f};
float white[4] = {1.0f, 1.0f, 1.0f, 1.0f};
float blue[4] = {0.0f, 0.45f, 0.8f, 1.0f};

unsigned int picked = 0;



void changeSize(int ww, int hh) {
    float ratio;
    w = ww;
    h = hh;
    if(h == 0)
        h = 1;
    ratio = 1.0f * w / h;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, w, h);
    gluPerspective(45, ratio, 1, 1000);
    glMatrixMode(GL_MODELVIEW);
}



void drawSnowMan() {
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, white);
    // BODY
    glTranslatef(0.0f, 0.75f, 0.0f);
    glutSolidSphere(0.75f, 20, 20);
    // HEAD
    glTranslatef(0.0f, 1.0f, 0.0f);
    glutSolidSphere(0.25f, 20, 20);
    // EYES
    glPushMatrix();
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, black);
    glTranslatef(0.05f, 0.10f, 0.18f);
    glutSolidSphere(0.05f, 10, 10);
    glTranslatef(-0.1f, 0.0f, 0.0f);
    glutSolidSphere(0.05f, 10, 10);
    glPopMatrix();
    // NOSE
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, orange);
    glRotatef(0.0f, 1.0f, 0.0f, 0.0f);
    glutSolidCone(0.08f, 0.5f, 10, 2);
}



void drawSnowMan(int code) {
    float color = code / 255.0f;
    glColor3f(color, color, color);
    // BODY
    glTranslatef(0.0f, 0.75f, 0.0f);
    glutSolidSphere(0.75f, 20, 20);
    // HEAD
    glTranslatef(0.0f, 1.0f, 0.0f);
    glutSolidSphere(0.25f, 20, 20);
    // EYES
    glPushMatrix();
    glTranslatef(0.05f, 0.10f, 0.18f);
    glTranslatef(-0.1f, 0.0f, 0.0f);
    glutSolidSphere(0.05f, 10, 10);
    glPopMatrix();
    // NOSE
    glRotatef(0.0f, 1.0f, 0.0f, 0.0f);
    glutSolidCone(0.08f, 0.5f, 10, 2);
}



void renderText(std::string const& text) {
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    gluOrtho2D(0, 2, h, 0);
    glMatrixMode(GL_MODELVIEW);
    glDisable(GL_DEPTH_TEST);
    glPushMatrix();
    glLoadIdentity();
    glRasterPos2d(10, 20);
    for(char c : text)
        glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18, c);
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
    glEnable(GL_DEPTH_TEST);
    std::cout << text << '\n';
}



void renderScene(void) {
    int times;
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    gluLookAt(camX, camY, camZ, 0.0, 0.0, 0.0, 0.0f, 1.0f, 0.0f);
    float dir[4] = {1.0f, 1.0f, 1.0f, 1.0f};
    glLightfv(GL_LIGHT0, GL_POSITION, dir);
    // GROUND 
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, blue);
    glPushMatrix();
    glTranslatef(0.0f, 1.0f, 0.0f);
    glutSolidTeapot(1);
    glPopMatrix();
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, green);
    glBegin(GL_QUADS);
    glNormal3f(0.0, 1.0f, 0.0f);
    glVertex3f(-100.0f, 0.0f, -100.0f);
    glVertex3f(-100.0f, 0.0f, 100.0f);
    glVertex3f(100.0f, 0.0f, 100.0f);
    glVertex3f(100.0f, 0.0f, -100.0f);
    glEnd();
    // SNOWMEN
    for(int i = 0 ; i < 2 ; i++)
        for(int j = 0 ; j < 2 ; j++) {
            glPushMatrix();
            glTranslatef(i * 5.0 - 2.5f, 0, j * 5.0 - 2.5f);
            drawSnowMan();
            glPopMatrix();
        }
    renderText(text);
    frame++;
    times = glutGet(GLUT_ELAPSED_TIME);
    if(times - timebase > 1000) {
        sprintf(s, "FPS:%4.2f", frame * 1000.0 / (times - timebase));
        timebase = times;
        frame = 0;
        glutSetWindowTitle(s);
    }
    glutSwapBuffers();
}



unsigned char picking(int x, int y) {
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();
    gluLookAt(camX, camY, camZ, 0.0, 0.0, 0.0, 0.0f, 1.0f, 0.0f);
    glDepthFunc(GL_EQUAL);
    //SNOWMEN
    for(int i = 0 ; i < 2 ; i++)
        for(int j = 0 ; j < 2 ; j++) {
            glPushMatrix();
            float cod = static_cast<float>(i * 2 + j + 1);
            glTranslatef(i * 5.0 - 2.5f, 0, j * 5.0 - 2.5f);
            drawSnowMan(cod);
            glPopMatrix();
        }
    glDepthFunc(GL_LESS);
    GLint viewport[4];
    unsigned char res[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    glReadPixels(x, viewport[3] - y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, res);
    glReadPixels(x, viewport[3] - y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, res);
    glEnable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
    return res[0];
}



void processNormalKeys(unsigned char key, int x, int y) {
    switch(key) {
        case 27:
            exit(0);
        case 'c':
            printf("Camera : %f %f %f\n", alpha, beta, r);
            break;
    }
}



void processMouseButtons(int button, int state, int xx, int yy) {
    printf("%d %d\n", xx , yy);
    if(state == GLUT_DOWN) {
        startX = xx;
        startY = yy;
        if(button == GLUT_LEFT_BUTTON)
            tracking = 1;
        else if(button ==  GLUT_RIGHT_BUTTON)
            tracking = 2;
        else {
            tracking = 0;
            picked = picking(xx, yy);
            if(picked) {
                std::stringstream ss;
                ss << "Picked Snowman Number " << picked;
                text = ss.str();
            } else 
                text = "Nothing Selected\n";
            glutPostRedisplay();
        }
    } else if (state == GLUT_UP) {
        if(tracking == 1) {
            alpha += (xx - startX);
            beta += (yy - startY);
        } else if (tracking == 2) {
            r -= yy - startY;
            if(r < 3)
                r = 3.0;
        }
        tracking = 0;
    }
}



void processMouseMotion(int xx, int yy) {
    int deltaX, deltaY;
    int alphaAux, betaAux;
    int rAux;
    if(!tracking)
        return;
    deltaX = xx - startX;
    deltaY = yy - startY;
    if(tracking == 1) {
        alphaAux = alpha + deltaX;
        betaAux = beta + deltaY;
        if(betaAux > 85.0)
            betaAux = 85.0;
        else if(betaAux < -85.0)
            betaAux = -85.0;
        rAux = r;
    } else if(tracking == 2) {
        alphaAux = alpha;
        betaAux = beta;
        rAux = r - deltaY;
        if(rAux < 3)
            rAux = 3; 
    }
    camX = rAux * sin(alphaAux * 3.14 / 180.0) * cos(betaAux * 3.14 / 180.0);
    camZ = rAux * cos(alphaAux * 3.14 / 180.0) * cos(betaAux * 3.14 / 180.0);
    camY = rAux * sin(betaAux * 3.14 / 180.0);
    glutPostRedisplay();
}



void init() {
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    camX = r * sin(alpha * 3.14 / 180.0) * cos(beta * 3.14 / 180.0);
    camZ = r * cos(alpha * 3.14 / 180.0) * cos(beta * 3.14 / 180.0);
    camY = r * sin(beta * 3.14 / 180.0);
}



int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(512, 512);
    glutCreateWindow("Gaia: Picking SnowMen from CG@DI");
    glutDisplayFunc(renderScene);
    glutReshapeFunc(changeSize);
    glutKeyboardFunc(processNormalKeys);
    glutMouseFunc(processMouseButtons);
    glutMotionFunc(processMouseMotion);
    init();
    glutMainLoop();
    return 0;
}