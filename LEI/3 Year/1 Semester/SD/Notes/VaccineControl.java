import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class VaccineControl {

    // RESOURCES
    int NUM;
    int numFlask;
    int ticket;
    int uInWait;
    int turn;
    int vaccGroup;
    // LOCKS/CONDITION
    Lock l;
    Condition wait;

    public VaccineControl() {
        this.NUM = 5;
        this.numFlask = 0;
        this.ticket = 0;
        this.uInWait = 0;
        this.turn = 0;
        this.vaccGroup = 0;
        this.l = new ReentrantLock();
        this.wait = l.newCondition();
    }

    void pedirParaVacinar() throws InterruptedException {
        int myTicket;
        try {
            l.lock();
            myTicket = this.ticket++;
            while(myTicket > turn || numFlask == 0 || uInWait < NUM) this.wait.await();
            turn++;
            vaccGroup++;
            this.wait.signalAll();
            if(vaccGroup == NUM) {
                uInWait -= NUM;
                numFlask--;
                vaccGroup = 0;
            }
        } finally {
            l.unlock();
        }
    }

    void fornecerFrascos(int frascos) {
        try {
            l.lock();
            numFlask += frascos;
            this.wait.signalAll();
        } finally {
            l.unlock();
        }
    }
}

class Handler implements Runnable {

    Socket s;
    VaccineControl v;
    DataInputStream in;
    DataOutputStream out;

    public Handler(Socket s, VaccineControl v) throws IOException {
        this.s = s;
        this.v = v;
        this.in = new DataInputStream(new BufferedInputStream(s.getInputStream()));
        this.out = new DataOutputStream(new BufferedOutputStream(s.getOutputStream()));
    }

    public void run() {
        String op;
        try {
            out.writeUTF("Pretende vacinar ou fornecer?");
            out.flush();
            op = in.readUTF();
            switch(op) {
                case "vacinar" : out.writeUTF("AGURADE");
                                 out.flush();
                                 v.pedirParaVacinar();
                                 out.writeUTF("VACINADO");
                                 out.flush();
                case "fornecer" : int nF = in.readInt();
                                  v.fornecerFrascos(nF);
                                  out.writeUTF("Recebidos " + nF + " frascos");
                                  out.flush();
            }
            out.flush();
            out.close();
            in.close();
            s.close();
        } catch (Exception ignored) {}
    }
}

class Server {
    public static void main(String[] args) throws IOException {
        VaccineControl v = new VaccineControl();
        ServerSocket ss = new ServerSocket(1234);
        while(true) {
            Socket s = ss.accept();
            Thread t = new Thread(new Handler(s,v));
            t.start();
        }
    }
}
