package TP_05.src;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Coop {

    Lock l = new ReentrantLock();

    private class Product {
        
        Condition cond = l.newCondition();
        int q = 0;

        public void consume() throws InterruptedException {
            l.lock();
            Product p = new Product();
            try {
                while(p.q == 0)
                    p.cond.await();
                p.q --;
                p.cond.signal();
            } finally {
                l.unlock();
            }
        }

    }

    private Map<String,Product> m = new HashMap<String,Product>();

    private Product get(String s) {
        Product product;
        l.lock();
        try {
            product = m.get(s);
            if(product != null)
                return product;
        } finally {
            l.unlock();
        }
        product = new Product();
        m.put(s , product);
        return product;
    }

    public void supply(String s, int q) {
        l.lock();
        Product product = get(s);
        product.q += q;
        product.cond.signalAll();
        l.unlock();
    }

    public void consume(String[] a) throws InterruptedException {
        l.lock();
        for(String x : a) {
            if(!this.m.containsKey(x))
                continue;
            m.get(x).consume(); 
            System.out.println("Item: " + x + " Stock Decreased");
        }
        l.unlock();
    }
    
}
