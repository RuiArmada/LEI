package TP_01.src.exercise_2;

public class Main {

    public static void main(String[] args) throws InterruptedException {
        Thread[] t = new Thread[10];
        Bank bk = new Bank();
        System.out.println("Initiating Balance Calculation...");
        for(int i = 0 ; i < 10 ; i++) {
            t[i] = new Thread(new Deposits(bk, 100, 1000));
            t[i].start();
        }
        for(int i = 0 ; i < 10 ; i++)
            t[i].join();
    }
    
}
