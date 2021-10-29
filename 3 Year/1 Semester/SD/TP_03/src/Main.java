package TP_03.src;

import java.util.Random;

class Move implements Runnable {
    
    Bank3 bk;
    int n_acc;

    public Move(Bank3 bk, int n_acc) {
        this.bk = bk;
        this.n_acc = n_acc;
    }

    public void run() {
        final int moves = 100000;
        int from, to;
        Random rand = new Random();
        for(int i = 0 ; i < moves ; i++) {
            from = rand.nextInt(n_acc);
            while((to = rand.nextInt(n_acc)) == from);
            bk.transfer(from, to, 1);
        }
    }

}

public class Main {

    public static void main(String[] args) throws InterruptedException {
        final int tam = 10;
        Bank3 b = new Bank3();
        for(int i = 0 ; i < tam ; i++)
            b.create(1000);
        System.out.println(b.total(new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
        Thread t1 = new Thread(new Move(b,10));
        Thread t2 = new Thread(new Move(b,10));
        t1.start();
        t2.start();
        t1.join();
        t1.join();
        System.out.println(b.total(new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
    }
    
}
