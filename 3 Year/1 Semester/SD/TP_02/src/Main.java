package TP_02.src;

import java.util.Random;

class Move implements Runnable {

    Bank2 bk;
    int n_Acc;

    public Move(Bank2 bk, int n_Acc) {
        this.bk = bk;
        this.n_Acc = n_Acc;
    }

    public void run() {
        final int moves = 100000;
        int from, to;
        Random rand = new Random();
        for(int i = 0 ; i < moves ; i++) {
            from = rand.nextInt(n_Acc);
            while((to = rand.nextInt(n_Acc)) == from);
            bk.transfer(from,to,1);
        }
    }

}

public class Main {

    public static void main(String[] args) throws InterruptedException {
        final int tam = 10;
        Bank2 b = new Bank2(tam);
        for(int i = 0 ; i < tam ; i++)
            b.deposit(i,1000);
        System.out.println(b.total());
        Thread t1 = new Thread(new Move(b,10));
        Thread t2 = new Thread(new Move(b,10));
        t1.start();
        t2.start();
        t1.join();
        t2.join();
        System.out.println(b.total());
    }
    
}
