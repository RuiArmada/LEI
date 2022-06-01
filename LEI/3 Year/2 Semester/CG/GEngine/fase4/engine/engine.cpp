#include <stdlib.h>

#include <IL/il.h>

#ifdef _APPLE_
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#endif

#define _USE_MATH_DEFINES
#include <math.h>

#include "algebra.hpp"
#include "parser.hpp"

using namespace tinyxml2;
using std::vector;

XMLDocument doc;

vector<group> scene;
vector<vector<GLuint>> buffers;
vector<vector<GLuint>> normals;
vector<vector<GLuint>> textBuffers;

vector<GLuint> textures;

struct light {
    std::array<float, 4> pos;
    std::array<float, 4> dir;
    float angle;
};

vector<light> lights;

double alpha = -M_PI_4, beta = M_PI_4 / 2, radius = 100.0f;
double camX, camY, camZ;
double dirX, dirY, dirZ;

void spherical2Cartesian() {

	camX = radius * cos(beta) * sin(alpha);
	camY = radius * sin(beta);
	camZ = radius * cos(beta) * cos(alpha);
}

bool FPS = false, VBOs = true, filledSolids = true;

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
        sprintf(title, "G.A.I.A | %lf FPS", fps);
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

int getLightN(int nLight) {
    int CLight;
    switch (nLight) {
        case 0: CLight = GL_LIGHT0; break;
        case 1: CLight = GL_LIGHT1; break;
        case 2: CLight = GL_LIGHT2; break;
        case 3: CLight = GL_LIGHT3; break;
        case 4: CLight = GL_LIGHT4; break;
        case 5: CLight = GL_LIGHT5; break;
        case 6: CLight = GL_LIGHT6; break;
        case 7: CLight = GL_LIGHT7; break;
        default: puts("Error - too many light sources."); exit(1);
    }
    return CLight;
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

    for(size_t lighti = 0; lighti < lights.size(); lighti++) {
        int CLight = getLightN(lighti);
        struct light lightx = lights[lighti];
        bool hasDir = lightx.angle != -1;
        glLightfv(CLight, GL_POSITION, lightx.pos.data());
        if (hasDir) {
            glLightfv(CLight, GL_SPOT_DIRECTION, lightx.dir.data());
            glLightfv(CLight, GL_SPOT_CUTOFF, &lightx.angle);
        }
    }

    glPolygonMode(GL_FRONT_AND_BACK, filledSolids ? GL_FILL : GL_LINE);

    size_t j = 0;

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

        vector<solidAttribs> & attribs = scene[group].solidAttributes;
        
        if(VBOs) {
            vector<GLuint> & buffersG = buffers[group];
            vector<GLuint> & normalsG = normals[group];
            vector<GLuint> & texturesG = textBuffers[group];

            for (unsigned long i = 0; i < buffers[group].size(); i++) {
                glPushAttrib(GL_LIGHTING_BIT);
                glBindBuffer(GL_ARRAY_BUFFER, buffersG[i]);
                glVertexPointer(3, GL_DOUBLE, 0, 0);

                if(normalsG.size() > i) {
                    glBindBuffer(GL_ARRAY_BUFFER, normalsG[i]);
                    glNormalPointer(GL_DOUBLE, 0, 0);
                }

                if(attribs[i].hasAmbient)
                    glMaterialfv(GL_FRONT, GL_AMBIENT, attribs[i].ambient.data());

                if(attribs[i].hasSpecular) 
                    glMaterialfv(GL_FRONT, GL_SPECULAR, attribs[i].specular.data());

                if(attribs[i].hasDiffuse) 
                    glMaterialfv(GL_FRONT, GL_AMBIENT, attribs[i].diffuse.data());

                if(attribs[i].hasEmissive) 
                    glMaterialfv(GL_FRONT, GL_EMISSION, attribs[i].emissive.data());

                if(!attribs[i].textureFile.empty())
                    glBindTexture(GL_TEXTURE_2D, textures[j++]);

                if(texturesG.size() > i) {
                    glBindBuffer(GL_ARRAY_BUFFER, texturesG[i]);
                    glTexCoordPointer(2, GL_DOUBLE, 0, 0);
                }

                glDrawArrays(GL_TRIANGLES, 0, scene[group].numVertices);
                glBindTexture(GL_TEXTURE_2D, 0);
                glPopAttrib();
            }
        }
        else {
            vector<std::array<vector<Point>,3>> & solids = scene[group].solids;
            for (unsigned long i = 0; i < solids.size(); i++) {
                std::vector<Point> & solid = solids[i][0];
                
                glPushAttrib(GL_LIGHTING_BIT);

                if(!attribs[i].textureFile.empty())
                    glBindTexture(GL_TEXTURE_2D, textures[j++]);
                
                glBegin(GL_TRIANGLES);
                
                if(attribs[i].hasAmbient)
                    glMaterialfv(GL_FRONT, GL_AMBIENT, attribs[i].ambient.data());

                if(attribs[i].hasSpecular) 
                    glMaterialfv(GL_FRONT, GL_SPECULAR, attribs[i].specular.data());

                if(attribs[i].hasDiffuse) 
                    glMaterialfv(GL_FRONT, GL_AMBIENT, attribs[i].diffuse.data());

                if(attribs[i].hasEmissive) 
                    glMaterialfv(GL_FRONT, GL_EMISSION, attribs[i].emissive.data());

                for (unsigned long j = 0; j < solid.size(); j++) {
                    if (solids[i][1].size() > 0) {
                        glNormal3d(solids[i][1][j].x(), solids[i][1][j].y(), solids[i][1][j].z());
                        if (solids[i][2].size() > 0) {
                            glTexCoord2d(solids[i][2][j].x(), solids[i][2][j].y());
                        }
                    }
                    glVertex3d(solid[j].x(), solid[j].y(), solid[j].z());
                }
                
                glEnd();
                
                glPopAttrib();
                
                glBindTexture(GL_TEXTURE_2D, 0);
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

            if (alpha < 0) alpha += M_PI * 2;
            else if (alpha > M_PI * 2) alpha -= M_PI * 2;
            if (beta < - M_PI_2) beta += M_PI * 2;
            else if (beta > (3 * M_PI_2)) beta -= M_PI * 2;

            spherical2Cartesian();
        }
        glutPostRedisplay();
    }
}

#pragma endregion

void initVBOs() {
    for(size_t group = 0; group < scene.size(); group++) {
        vector<std::array<vector<Point>,3>> &solids = scene.at(group).solids;
        
        vector<GLuint> buffersG(solids.size());
        vector<GLuint> normalsG;
        vector<GLuint> texturesG;
        
        glGenBuffers(buffersG.size(), buffersG.data());
        
        for(size_t solid = 0; solid < solids.size(); solid++) {
            vector<double> vertices;
            vector<double> normalB;
            vector<double> textureB;

            bool hasNormals = solids[solid][1].size() > 0;
            bool hasTextures = solids[solid][2].size() > 0;

            for(size_t point = 0; point < solids[solid][0].size(); point++) {
                Point coords = solids[solid][0][point];

                vertices.push_back(coords.x());
                vertices.push_back(coords.y());
                vertices.push_back(coords.z());

                if (hasNormals) {
                    Point normal = solids[solid][1][point];
                    
                    normalB.push_back(normal.x());
                    normalB.push_back(normal.y());
                    normalB.push_back(normal.z());
                    
                    if(hasTextures) {

                        Point texture = solids[solid][2][point];

                        textureB.push_back(texture.x());
                        textureB.push_back(texture.y());
                    }
                }
            }
            glBindBuffer(GL_ARRAY_BUFFER, buffersG.at(solid));
            glBufferData(GL_ARRAY_BUFFER, vertices.size() * sizeof(double), vertices.data(), GL_STATIC_DRAW);

            if(hasNormals) {
                GLuint b;
                glGenBuffers(1, &b); 
                glBindBuffer(GL_ARRAY_BUFFER, b);
                glBufferData(GL_ARRAY_BUFFER, normalB.size() * sizeof(double), normalB.data(), GL_STATIC_DRAW);
                normalsG.push_back(b);
            }

            if(hasTextures) {
                GLuint b;
                glGenBuffers(1, &b); 
                glBindBuffer(GL_ARRAY_BUFFER, b);
                glBufferData(GL_ARRAY_BUFFER, textureB.size() * sizeof(double), textureB.data(), GL_STATIC_DRAW);
                texturesG.push_back(b);
            }
        }
        buffers.push_back(buffersG);
        normals.push_back(normalsG);
        textBuffers.push_back(texturesG);
    }
}

void loadTextures() {
    size_t i = 0;
    for(struct group groupX : scene) {
        for(solidAttribs attribs : groupX.solidAttributes) {
            if (!attribs.textureFile.empty()) {
                unsigned int t, tw, th;
                unsigned char *texData;
                ilGenImages(1, &t);
                ilBindImage(t);
                if (!ilLoadImage((ILstring)attribs.textureFile.c_str())) {
                    if (!ilLoadImage((ILstring)std::string("../").append(attribs.textureFile).c_str()))
                        printf("Error - texture file %s not found.\n", attribs.textureFile.c_str());
                }
                tw = ilGetInteger(IL_IMAGE_WIDTH);
                th = ilGetInteger(IL_IMAGE_HEIGHT);
                ilConvertImage(IL_RGBA, IL_UNSIGNED_BYTE);
                texData = ilGetData();

                GLuint tex;

                glGenTextures(1, &tex);

                glBindTexture(GL_TEXTURE_2D, tex);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

                glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tw, th, 0, GL_RGBA, GL_UNSIGNED_BYTE, texData);
                glGenerateMipmap(GL_TEXTURE_2D);

                textures.push_back(tex);
                i++;
            }
        }
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

void printInfo() {
    puts("\nControls for 3rd person camera: \n\
    WASD / Left mouse click + mouse movement - rotate\n\
    Q - zoom in\n\
    E - zoom out\n\
    \n\
Controls for 1st person camera:\n\
    WASD - move\n\
    Left mouse click + mouse movement - look around\n\
    \n\
V - switch between 1st and 3rd person modes\n\
Right mouse button - menu for toggling VBOs and filled models\n");
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

    XMLElement* sceneXML = doc.FirstChildElement("scene");
    if(sceneXML == NULL) {
        puts("No scene found.");
        return 1;
    }

    XMLElement * lightsXML = sceneXML->FirstChildElement("lights");

    GLdouble idM[] = {1,0,0,0,
                      0,1,0,0,
                      0,0,1,0,
                      0,0,0,1};

    int colors[3] = {255, 255, 255};

    struct group initGroup;
    memcpy(&initGroup.colors, colors, 3 * sizeof(int));


    parseGroup(scene, sceneXML, idM, initGroup);

    //init GLUT and the Window
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
    glutInitWindowPosition(100,100);
    glutInitWindowSize(800,800);
    glutCreateWindow("G.A.I.A");

    //Required callback registry
    glutDisplayFunc(renderScene);
    glutIdleFunc(renderScene);
    glutKeyboardFunc(keyboardFunc);
	glutReshapeFunc(changeSize);
    glutMouseFunc(mouseFunc);
    glutMotionFunc(motionFunc);

	glewInit();
	glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    initVBOs();

    //OpenGL settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glEnable(GL_RESCALE_NORMAL);
    glEnable(GL_COLOR_MATERIAL);

    glEnable(GL_TEXTURE_2D);

    if (lightsXML) {
        glEnable(GL_LIGHTING);

        GLfloat dark[4] = {0.3,0.3,0.3,1.0};
        GLfloat white[4] = {1.0,1.0,1.0,1.0};

        XMLElement * lightXML = lightsXML->FirstChildElement();
        int nLight = 0;
        while (lightXML) {
            int CLight = getLightN(nLight);
            glEnable(CLight);

            glLightfv(CLight, GL_AMBIENT, dark);
            glLightfv(CLight, GL_DIFFUSE, white);
            glLightfv(CLight, GL_SPECULAR, white);

            float posx = lightXML->FloatAttribute("posX");
            float posy = lightXML->FloatAttribute("posY");
            float posz = lightXML->FloatAttribute("posZ");
            float dirx = lightXML->FloatAttribute("dirX");
            float diry = lightXML->FloatAttribute("dirY");
            float dirz = lightXML->FloatAttribute("dirZ");
            float angle = lightXML->FloatAttribute("angle");
            const char * type = lightXML->Attribute("type");

            struct light lightx;

            if(!strcmp(type, "POINT")) {
                lightx.pos = {posx,posy,posz,1};
                lightx.angle = -1;
            }
            else if(!strcmp(type, "DIRECTIONAL")) {
                lightx.pos = {dirx,diry,dirz,0};
                lightx.angle = -1;
            }
            else if(!strcmp(type, "SPOT")) {
                lightx.pos = {posx,posy,posz,1};
                lightx.dir = {dirx,diry,dirz,0};
                lightx.angle = angle;
            }

            lights.push_back(lightx);

            nLight++;
            lightXML = lightXML->NextSiblingElement();
        }
    }

	ilInit();
    loadTextures();

    spherical2Cartesian();

    glutCreateMenu(menu);
    glutAddMenuEntry("Disable VBOs",1);
    glutAddMenuEntry("Line models",2); 
    glutAttachMenu(GLUT_RIGHT_BUTTON);

    timebase = glutGet(GLUT_ELAPSED_TIME); 

    #ifdef _WIN32
    system("cls");
    #else
    system("clear");
    #endif
    
    printInfo();

    //enter GLUT's main cycle
    glutMainLoop();

    return 0;
}