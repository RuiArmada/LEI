package TP_01.src.exercise_1;

public class Incrementer implements Runnable {

    private long x;

    public Incrementer(long y) {
        this.x = y;
    }

    public void run() {
        for(long y = 1 ; y <= this.x ; y++)
            System.out.println(y);
    }

}