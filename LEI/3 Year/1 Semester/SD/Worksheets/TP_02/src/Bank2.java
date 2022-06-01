package TP_02.src;

import java.util.Arrays;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Bank2 {
    
    private static class Account {

        Lock lock = new ReentrantLock();
        
        private int balance;

        Account(int balance) {
            this.balance = balance;
        }

        int balance() {
            return balance;
        }

        boolean deposit(int val) {
            try {
                lock.lock();
                balance += val;
                return true;
            } finally {
                lock.unlock();
            }
        }

        boolean withdraw(int val) {
            try {
                lock.lock();
                if(val > balance)
                    return false;
                balance -= val;
                return true;
            } finally {
                lock.unlock();
            }
        }

    }

    private int slots;
    private Account[] a;

    public Bank2(int n) {
        slots = n;
        a = new Account[slots];
        for (int i = 0 ; i < slots ; i++)
            a[i] = new Account(0);
    }

    public int balance(int id) {
        if(id < 0 || id >= slots)
            return 0;
        return a[id].balance();
    }

    boolean deposit(int id, int val) {
        if(id < 0 || id >= slots)
            return false;
        return a[id].deposit(val);
    }

    public boolean withdraw(int id, int val) {
        if(id < 0 || id >= slots)
            return false;
        return a[id].withdraw(val);
    }

    public boolean transfer(int from, int to, int val) {
        if(!withdraw(from, val))
            return false;
        return deposit(to,val);
    }

    int total() {
        return Arrays.stream(this.a)
                     .mapToInt(Account::balance)
                     .sum();
    }
}
