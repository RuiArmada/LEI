package UMAirlines.src;

import java.io.IOException;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Class that handles de comunication between client and server.
 */
public class Demultiplexer {

    private final Connect cn;
    private final ReentrantLock lck = new ReentrantLock();
    private IOException e = null;
    private final Map<Integer,FrameValue> m = new HashMap<>();

    private class FrameValue {
        
        int waits = 0;
        Queue<byte[]> queue = new ArrayDeque<>();
        Condition cond = lck.newCondition();

        public FrameValue() {} 

    }

    /**
     * Makes a connection
     * @param cn Connection
     */
    public Demultiplexer(Connect cn) {
        this.cn = cn;
    }

    /**
     * Creates and starts a new thread to liberate the main one
     */
    public void start() {
        new Thread(() -> {
            try {
                while(true) {
                    Frames f = cn.recieve();
                    lck.lock();
                    try {
                        FrameValue fv = m.get(f.tag);
                        if(fv == null) {
                            fv = new FrameValue();
                            m.put(f.tag, fv);
                        }
                        fv.queue.add(f.data);
                        fv.cond.signal();
                    } finally {
                        lck.unlock();
                    }
                }
            } catch(IOException ex) {
                e = ex;
            }
        }).start();
    }

    /**
     * Sends a frame
     * @param f Frame
     * @throws IOException
     */
    public void send(Frames f) throws IOException {
        cn.send_frame(f);
    }

    /**
     * Send a frame with info
     * @param tag Frame's id tag
     * @param name Name of the user that sent the frame
     * @param data Information contained in the frame
     * @throws IOException
     */
    public void send(int tag, String name, byte[] data) throws IOException {
        cn.send_f_Info(tag, name, data);
    }

    /**
     * Stays blocked until content exists in the frame
     * @param tag Frame's id
     * @return array of bytes
     * @throws IOException
     * @throws InterruptedException
     */
    public byte[] recieve(int tag) throws IOException, InterruptedException {
        lck.lock();
        FrameValue fv;
        try {
            fv = m.get(tag);
            if(fv == null) {
                fv = new FrameValue();
                m.put(tag, fv);
            }
            fv.waits ++;
            while(true) {
                if(!fv.queue.isEmpty()) {
                    fv.waits --;
                    byte[] reply = fv.queue.poll();
                    if(fv.waits == 0 && fv.queue.isEmpty()) {
                        m.remove(tag);
                    }
                    return reply;
                }
                if(e != null) {
                    throw e;
                }
                fv.cond.await();
            }
        } finally {
            lck.unlock();
        }
    }

    public void close() throws IOException {
        cn.close();
    }
    
}
