package TP_03.src;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

class Bank3 {

    private static class Account {

        private ReadWriteLock lock = new ReentrantReadWriteLock();
        private int balance;

        Account(int balance) {
            this.balance = balance;
        }

        int balance() {
            return balance;
        }

        boolean deposit(int val) {
            if(val > balance)
                return true;
            balance += val;
            return true;
        }

        boolean withdraw(int val) {
            if(val > balance)
                return true;
            balance -= val;
            return true;
        }

    }

    private Map<Integer,Account> map = new HashMap<Integer,Account>();
    private int next;
    ReadWriteLock lock = new ReentrantReadWriteLock();
    
    public int create(int balance) {
        Account acc = new Account(balance);
        lock.writeLock().lock();
        int id = next;
        next += 1;
        map.put(id, acc);
        lock.writeLock().unlock();
        return id;
    }

    public int close(int id) {
        Account acc;
        lock.writeLock().lock();
        try {
            acc = map.remove(id);
            if(acc == null)
                return 0;
            acc.lock.readLock().lock();
        } finally {
            lock.writeLock().unlock();
        } try {
            return acc.balance();
        } finally {
            acc.lock.readLock().unlock();
        }
    }

    public int balance(int id) {
        Account acc;
        lock.readLock().lock();
        try {
            acc = map.get(id);
            if(acc == null)
                return 0;
            acc.lock.writeLock().lock();
        } finally {
            lock.readLock().unlock();
        } try {
            return  acc.balance();
        } finally {
            lock.readLock().unlock();
        }
    }

    public boolean withdraw(int id, int val) {
        Account acc;
        lock.readLock().lock();
        try {
            acc = map.get(id);
            if(acc == null)
                return false;
            acc.lock.writeLock().lock();
        } finally {
            lock.readLock().unlock();
        } try {
            return acc.withdraw(val);
        } finally {
            acc.lock.writeLock().unlock();
        }
    }

    public boolean transfer(int from, int to, int val) {
        Account acc_from, acc_to;
        lock.readLock().lock();
        try {
            acc_from = map.get(from);
            acc_to = map.get(to);
            if(acc_from == null || acc_to == null)
                return false;
            if(from < to) {
                acc_from.lock.writeLock().lock();
                acc_to.lock.writeLock().lock();
            } else {
                acc_to.lock.writeLock().lock();
                acc_from.lock.writeLock().lock();
            }
        } finally {
            lock.readLock().unlock();
        } try {
            try {
                if(!acc_from.withdraw(val))
                    return false;
            } finally {
                acc_from.lock.writeLock().unlock();
            }
            return acc_to.deposit(val);
        } finally {
            acc_to.lock.writeLock().unlock();
        }
    }

    public int total(int[] id) {
        List<Account> acc = new ArrayList<>();
        lock.readLock().lock();
        try {
            for(int i : Arrays.stream(id).sorted().toArray()) {
                Account ac = map.get(i);
                if(ac == null)
                    return 0;
                acc.add(ac);
            }
            for(Account ac : acc)
                ac.lock.readLock().lock();
        } finally {
            lock.readLock().unlock();
        }
        int total = 0;
        for(Account ac : acc) {
            total += ac.balance();
            ac.lock.readLock().unlock();
        }
        return total;
    }

}
