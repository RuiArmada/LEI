#ifndef POINT_H
#define POINT_H

class Point {
    private:
        double _x;
        double _y;
        double _z;

    public:
        Point(double x, double y, double z);

        double x();
        double y();
        double z();

        Point invertX() ;
        Point invertY() ;
        Point invertZ() ;
        Point normalize() ;
};

#endif