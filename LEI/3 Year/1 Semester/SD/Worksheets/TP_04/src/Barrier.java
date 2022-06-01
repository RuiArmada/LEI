package TP_04.src;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Barrier {

    private Lock l =  new ReentrantLock();
    private Condition c = l.newCondition();
    private int ep;
    private int counter = 0;
    private final int N;

    Barrier(int N) {
        this.N = N;
        ep = 0;
    }

    void awaits() throws InterruptedException {
        l.lock();
        try {
            int ep = this.ep;
            counter ++;
            if(counter < N) {
                while(ep == this.ep)
                    c.await();
            } else {
                this.ep ++;
                counter = 0;
                c.signalAll();
            }
        } finally {
            l.unlock();
        }
    }

}

