package TP_01.src.exercise_2;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Bank {

    private static class Account {

        private int balance;

        Account(int balance) {
            this.balance = balance;
        }

        int balance() {
            return balance;
        }

        boolean deposit(int val) {
            balance += val;
            return true;
        }

    }

    Lock lock = new ReentrantLock();
    
    private Account savings = new Account(0);

    public int balance() {
        lock.lock();
        try {
            int i = savings.balance();
            return i;
        } finally {
            lock.unlock();
        }
    } 

    boolean deposit(int val) {
        lock.lock();
        try {
            boolean x = savings.deposit(val);
            return x;
        } finally {
            lock.unlock();
        }
    }

}
