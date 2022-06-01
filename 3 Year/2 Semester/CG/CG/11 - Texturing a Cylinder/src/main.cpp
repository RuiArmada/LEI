#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>

#ifdef __APPLE__
    #include <GLUT/glut.h>
#else 
    #include <GL/glew.h>
    #include <GL/glut.h>
#endif 

#include <IL/il.h>

#define _PI_ 3.14159

float alfa = 0.0f, beta = 0.70f, radius = 5.0f;
float camX, camY, camZ;

GLuint vertexCount, vertex,
       normals, texCoord,
       index, indCount,
       texIDCylinder, texIDFloor;

int timebase = 0, frame = 0, side = 16, mode = 1;

void convert() {
    camX = radius * cos(beta) * sin(alfa);
    camY = radius * sin(beta);
    camZ = radius * cos(beta) * cos(alfa);
}



void changeSize(int w, int h) {
    if(h == 0)
        h = 1;
    float ratio = w * 1.0 / h;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, w, h);
    gluPerspective(45, ratio, 0.1, 100);
    glMatrixMode(GL_MODELVIEW);
}



void Cylinder(float height, float rad, int sides) {
    float* v = (float*) malloc(sizeof(float) * 4 * 3 * 3 * sides);
    float* n = (float*) malloc(sizeof(float) * 4 * 3 * 3 * sides);
    float* t = (float*) malloc(sizeof(float) * 4 * 3 * 3 * sides);
    int vertex_aux = 0;
    float delta = 2.0f * _PI_ / sides;
    for(int i = 0 ; i < sides ; ++i) {

        // TOP
        n[vertex_aux * 3 + 0] = 0.0f;
        n[vertex_aux * 3 + 1] = 1.0f;
        n[vertex_aux * 3 + 2] = 0.0f;
        v[vertex_aux * 3 + 0] = 0.0f;
        v[vertex_aux * 3 + 1] = height / 2.0f;
        v[vertex_aux * 3 + 2] = 0.0f;
        t[vertex_aux * 2 + 0] = 0.4375f;
        t[vertex_aux * 2 + 1] = 0.1875f;

        vertex_aux++;
        n[vertex_aux * 3 + 0] = 0;
        n[vertex_aux * 3 + 1] = 1.0f;
        n[vertex_aux * 3 + 2] = 0;
        v[vertex_aux * 3 + 0] = rad * sin(i * delta);
        v[vertex_aux * 3 + 1] = height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos(i * delta);
        t[vertex_aux * 2 + 0] = 0.4375f + 0.1875 * sin(i * delta);
        t[vertex_aux * 2 + 1] = 0.1875f + 0.1875 * cos(i * delta);

        vertex_aux++;
        n[vertex_aux * 3 + 0] = 0;
        n[vertex_aux * 3 + 1] = 1.0f;
        n[vertex_aux * 3 + 2] = 0;
        v[vertex_aux * 3 + 0] = rad * sin((i + 1) * delta);
        v[vertex_aux * 3 + 1] = height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos((i + 1) * delta);
        t[vertex_aux * 2 + 0] = 0.4375f + 0.1875 * sin((i + 1) * delta);
        t[vertex_aux * 2 + 1] = 0.1875f + 0.1875 * cos((i + 1) * delta);

        // BODY
        vertex_aux++;
        n[vertex_aux * 3 + 0] = sin((i + 1) * delta);
        n[vertex_aux * 3 + 1] = 0.0f;
        n[vertex_aux * 3 + 2] = cos((i + 1) * delta);
        v[vertex_aux * 3 + 0] = rad * sin((i + 1) * delta);
        v[vertex_aux * 3 + 1] = height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos((i + 1) * delta);
        t[vertex_aux * 2 + 0] = (i + 1) / static_cast<float>(sides);
        t[vertex_aux * 2 + 1] = 1.0f;

        vertex_aux++;
        n[vertex_aux * 3 + 0] = sin(i * delta);
        n[vertex_aux * 3 + 1] = 0.0f;
        n[vertex_aux * 3 + 2] = cos(i * delta);
        v[vertex_aux * 3 + 0] = rad * sin(i * delta);
        v[vertex_aux * 3 + 1] = height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos(i * delta);
        t[vertex_aux * 2 + 0] = i / static_cast<float>(sides);
        t[vertex_aux * 2 + 1] = 1.0f;

        vertex_aux++;
        n[vertex_aux * 3 + 0] = sin(i * delta);
        n[vertex_aux * 3 + 1] = 0.0f;
        n[vertex_aux * 3 + 2] = cos(i * delta);
        v[vertex_aux * 3 + 0] = rad * sin(i * delta);
        v[vertex_aux * 3 + 1] = -height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos(i * delta);
        t[vertex_aux * 2 + 0] = i / static_cast<float>(sides);
        t[vertex_aux * 2 + 1] = 0.375f;

        vertex_aux++;
        n[vertex_aux * 3 + 0] = sin((i + 1) * delta);
        n[vertex_aux * 3 + 1] = 0.0f;
        n[vertex_aux * 3 + 2] = cos((i + 1) * delta);
        v[vertex_aux * 3 + 0] = rad * sin((i + 1) * delta);
        v[vertex_aux * 3 + 1] = -height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos((i + 1) * delta);
        t[vertex_aux * 2 + 0] = (i + 1) / static_cast<float>(sides);
        t[vertex_aux * 2 + 1] = 0.375f;

        vertex_aux++;    // alguns settings para OpenGL
        n[vertex_aux * 3 + 0] = sin((i + 1) * delta);
        n[vertex_aux * 3 + 1] = 0.0f;
        n[vertex_aux * 3 + 2] = cos((i + 1) * delta);
        v[vertex_aux * 3 + 0] = rad * sin((i + 1) * delta);
        v[vertex_aux * 3 + 1] = height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos((i + 1) * delta);
        t[vertex_aux * 2 + 0] = (i + 1) / static_cast<float>(sides);
        t[vertex_aux * 2 + 1] = 1.0f;

        vertex_aux++;
        n[vertex_aux * 3 + 0] = sin(i * delta);
        n[vertex_aux * 3 + 1] = 0.0f;
        n[vertex_aux * 3 + 2] = cos(i * delta);
        v[vertex_aux * 3 + 0] = rad * sin(i * delta);
        v[vertex_aux * 3 + 1] = -height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos(i * delta);
        t[vertex_aux * 2 + 0] = i / static_cast<float>(sides);
        t[vertex_aux * 2 + 1] = 0.375f;

        // BASE
        vertex_aux++;
        n[vertex_aux * 3 + 0] = 0.0f;
        n[vertex_aux * 3 + 1] = -1.0f;
        n[vertex_aux * 3 + 2] = 0.0f;
        v[vertex_aux * 3 + 0] = 0.0f;
        v[vertex_aux * 3 + 1] = -height / 2.0f;
        v[vertex_aux * 3 + 2] = 0.0f;
        t[vertex_aux * 2 + 0] = 0.8125f;
        t[vertex_aux * 2 + 1] = 0.1875f;

        vertex_aux++;
        n[vertex_aux * 3 + 0] = 0.0f;
        n[vertex_aux * 3 + 1] = -1.0f;
        n[vertex_aux * 3 + 2] = 0.0f;
        v[vertex_aux * 3 + 0] = rad * sin((i + 1) * delta);
        v[vertex_aux * 3 + 1] = -height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos((i + 1) * delta);
        t[vertex_aux * 2 + 0] = 0.8125f + 0.1875 * sin((i + 1) * delta);
        t[vertex_aux * 2 + 1] = 0.1875f + 0.1875 * cos((i + 1) * delta);

        vertex_aux++;
        n[vertex_aux * 3 + 0] = 0.0f;
        n[vertex_aux * 3 + 1] = -1.0f;
        n[vertex_aux * 3 + 2] = 0.0f;
        v[vertex_aux * 3 + 0] = rad * sin(i * delta);
        v[vertex_aux * 3 + 1] = -height / 2.0f;
        v[vertex_aux * 3 + 2] = rad * cos(i * delta);
        t[vertex_aux * 2 + 0] = 0.8125f + 0.1875 * sin(i * delta);
        t[vertex_aux * 2 + 1] = 0.1875f + 0.1875 * cos(i * delta);

        vertex_aux++;
    }
    vertexCount = vertex_aux;
    glGenBuffers(1, &vertex);
    glBindBuffer(GL_ARRAY_BUFFER, vertex);
    glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vertexCount * 3, v, GL_STATIC_DRAW);
    glGenBuffers(1, &normals);
    glBindBuffer(GL_ARRAY_BUFFER, normals);
    glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vertexCount * 3, n, GL_STATIC_DRAW);
    glGenBuffers(1, &texCoord);
    glBindBuffer(GL_ARRAY_BUFFER, texCoord);
    glBufferData(GL_ARRAY_BUFFER, sizeof(float) * vertexCount * 2, t, GL_STATIC_DRAW);
    free(v);
    free(n);
    free(t);
}



void drawFloor(float Xmin, float Xmax, float Zmin, float Zmax) {
    glBindTexture(GL_TEXTURE_2D, texIDFloor);
    glBegin(GL_QUADS);
    glNormal3f(0, 1, 0);
    glVertex3f(Xmax, 0, Zmin);
    glTexCoord2d(1, 0);
    glVertex3f(Xmin, 0, Zmin);
    glTexCoord2d(0, 0);
    glVertex3f(Xmin, 0, Zmax);
    glTexCoord2d(0, 1);
    glVertex3f(Xmax, 0, Zmax);
    glTexCoord2d(1, 1);
    glEnd();
    glBindTexture(GL_TEXTURE_2D, 0);
}



void drawCylinder() {
    glBindTexture(GL_TEXTURE_2D, texIDCylinder);
    glBindBuffer(GL_ARRAY_BUFFER, vertex);
    glVertexPointer(3, GL_FLOAT, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, normals);
    glNormalPointer(GL_FLOAT, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, texCoord);
    glTexCoordPointer(2, GL_FLOAT, 0, 0);
    glDrawArrays(GL_TRIANGLES, 0, vertexCount);
    glBindTexture(GL_TEXTURE_2D, 0);
}



void renderScene(void) {
    float pos[4] = {1.0, 1.0, 1.0, 0.0};
    float fps;
    int timet;
    char s[64];
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    gluLookAt(camX, camY, camZ, 0.0, 0.0, 0.0, 0.0f, 1.0f, 0.0f);
    glLightfv(GL_LIGHT0, GL_POSITION, pos);
    float white[4] = {1, 1, 1, 1};
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, white);
    drawFloor(-5, 5, -5, 5);
    glTranslatef(0, 1, -1);
    drawCylinder();
    glTranslatef(0, 0, 2);
    drawCylinder();
    frame++;
    timet = glutGet(GLUT_ELAPSED_TIME);
    if (timet - timebase > 1000) {
        fps = frame * 1000.0 / (timet - timebase);
        timebase = timet;
        frame = 0;
        sprintf(s, "FPS: %f6.2", fps);
        glutSetWindowTitle(s);
    }
    glutSwapBuffers();
}



void processKeys(int key, int xx, int yy) {
    switch (key) {
        case GLUT_KEY_RIGHT:
            alfa -= 0.1;
            break;

        case GLUT_KEY_LEFT:
            alfa += 0.1;
            break;

        case GLUT_KEY_UP:
            beta += 0.1f;
            if (beta > 1.5f) beta = 1.5f;
            break;

        case GLUT_KEY_DOWN:
            beta -= 0.1f;
            if (beta < -1.5f) beta = -1.5f;
            break;

        case GLUT_KEY_PAGE_DOWN:
            radius -= 0.1f;
            if (radius < 0.1f) radius = 0.1f;
            break;

        case GLUT_KEY_PAGE_UP:
            radius += 0.1f;
            break;

        case GLUT_KEY_F1:
            mode = !mode;
            printf("mode: %d\n", mode);
            break;
    }
    convert();
    glutPostRedisplay();
}



void initGL() {
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    convert();
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glClearColor(0, 0, 0, 0);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glEnable(GL_TEXTURE_2D);
    Cylinder(2, 1, side);
}



int texture(std::string s) {
    unsigned int t, tw, th, texID;
    unsigned char* texData;
    ilInit();
    ilEnable(IL_ORIGIN_SET);
    ilOriginFunc(IL_ORIGIN_LOWER_LEFT);
    ilGenImages(1, &t);
    ilBindImage(t);
    ilLoadImage((ILstring) s.c_str());
    tw = ilGetInteger(IL_IMAGE_WIDTH);
    th = ilGetInteger(IL_IMAGE_HEIGHT);
    ilConvertImage(IL_RGBA, IL_UNSIGNED_BYTE);
    texData = ilGetData();
    glGenTextures(1, &texID);
    glBindTexture(GL_TEXTURE_2D, texID);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA,
        tw,
        th,
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        texData);
    glGenerateMipmap(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, 0);
    return texID;
}



int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(320, 320);
    glutCreateWindow("Gaia");
    glutDisplayFunc(renderScene);
    glutReshapeFunc(changeSize);
    glutSpecialFunc(processKeys);
    #ifndef __APPLE__
        glewInit();
    #endif
    initGL();
    texIDCylinder = texture("/images/Oil_Drum001h.jpg");
    texIDFloor = texture("/images/Concrete.jpg");
    glutMainLoop();
    return 1;
}