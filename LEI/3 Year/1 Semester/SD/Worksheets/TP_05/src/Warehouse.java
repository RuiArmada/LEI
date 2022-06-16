package TP_05.src;

import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

public class Warehouse {

    Lock l = new ReentrantLock();

    private class Product {

        Condition cond = l.newCondition();
        int q = 0;

    }
    
    private Map<String,Product> m = new HashMap<String,Product>();

    private Product get(String s) {
        Product product;
        product = m.get(s);
        if(product != null)
            return product;
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
        for(String s : Arrays.stream(a).sorted().collect(Collectors.toCollection(ArrayList::new))) {
            Product product = get(s);
            while(product.q == 0)
                product.cond.await();
            product.q -= 1;
        }
        l.unlock();
    }

}
