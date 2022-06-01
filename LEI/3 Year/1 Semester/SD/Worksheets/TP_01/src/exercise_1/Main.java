package TP_01.src.exercise_1;

public class Main {

    public static void main(String[] args) throws InterruptedException, NumberFormatException {
        Thread[] t = new Thread[5];
        long y = Integer.parseInt(args[0]);
        for(int i = 0 ; i < 5 ; i++) {
            t[i] = new Thread(new Incrementer(y));
            t[i].start();
        }
        for(int i = 0 ; i < 5 ; i++)
            t[i].join();
    }
    
}
