import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Meeting {

    Map<Integer,Integer> waiting; // (idList, nPeople)
    Lock waitingLock;

    int inRoom; // id of List inside the room
    Lock inRoomLock;

    int amountInRoom; // Amount of people inside the room
    Lock amountinRoomLock;

    Map<Integer,Condition> cond; // (idList,Condition), one condition per list to wake up only that list
    Lock condLock;

    public Meeting() {
        // RESOURCES
        this.waiting = new HashMap<>();
        this.inRoom = 0;
        this.amountInRoom = 0;
        this.cond = new HashMap<>();
        // LOCKS
        this.waitingLock = new ReentrantLock();
        this.amountinRoomLock = new ReentrantLock();
        this.amountinRoomLock = new ReentrantLock();
        this.condLock = new ReentrantLock();
    }

    void participa(int lista) throws InterruptedException {
        inRoomLock.lock();
        if(this.inRoom != 0) { // Means that the room is busy
            if(this.cond.containsKey(lista)) {   
                condLock.lock();
                Condition listCond = cond.get(lista);
                listCond.await();
            }
            condLock.unlock();
        }
        inRoomLock.unlock();
        if(this.inRoom != lista) { // New List is gonna enter the room
            inRoomLock.lock();
            this.inRoom = lista; // updates the list in the room
            inRoomLock.unlock();
        }
        // Updating that one person has entered the room
        amountinRoomLock.lock();
        amountInRoom ++;
        amountinRoomLock.unlock();
        //Removing person from waiting list
        waitingLock.lock();
        if(waiting.containsKey(lista)) {
            int ammountWaiting = waiting.get(lista);
            if(ammountWaiting == 1) waiting.remove(lista);
            else waiting.put(lista, ammountWaiting - 1);
        }
        waitingLock.unlock();
    }

    int getPriority() {
        int max = 0, res = 0;
        waitingLock.lock();
        for(Map.Entry<Integer, Integer> m : this.waiting.entrySet()) {
            if(max < m.getValue()) {
                max = m.getValue();
                res = m.getKey();
            }
        }
        waitingLock.unlock();
        return res;
    }

    void abandona(int lista) {
        inRoomLock.lock();
        int listIn = this.inRoom;
        if(lista == listIn) {
            amountinRoomLock.lock();
            amountInRoom--;
            // Checks if it is empty
            if(amountInRoom == 0) {
                // Empties the rrom
                inRoomLock.lock();
                inRoom = getPriority(); // Select next list to go in
                inRoomLock.unlock();
                // Signals a list to go in
                if(inRoom != 0) {
                    condLock.lock();
                    Condition c = cond.get(inRoom);
                    c.signalAll();
                    condLock.unlock();
                }
            }
            amountinRoomLock.unlock();
        }
        inRoomLock.unlock();
    }

    int naSala() {
        inRoomLock.lock();
        try {
            return this.inRoom;
        } finally {
            inRoomLock.unlock();
        }
    }

    int aEspera() {
        int total = 0;
        waitingLock.lock();
        for(Integer n : waiting.values()) total += n;
        waitingLock.unlock();
        return total;
    }

}

class Handler implements Runnable {

    Socket s;
    Meeting m;
    DataInputStream in;
    DataOutputStream out;

    public Handler(Socket s, Meeting m) throws IOException {
        this.s = s;
        this.m = m;
        this.in = new DataInputStream(new BufferedInputStream(s.getInputStream()));
        this.out = new DataOutputStream(new BufferedOutputStream(s.getOutputStream()));
    }

    public void run() {
        try {
           int nLista = in.readInt();
           m.participa(nLista);
           out.writeUTF("ENTREI");
           out.flush();
           String read;
           while((read = in.readUTF()) != "ABANDNONEI")
                if(read == "STATUS") 
                    out.writeUTF("In room: " + m.naSala() + "; Waiting: " + m.aEspera());
            m.abandona(nLista);
            out.flush();
            out.close();
            in.close();
            s.close();
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}

class Server {

    public static void main(String[] args) throws IOException {
        Meeting m = new Meeting();
        ServerSocket ss = new ServerSocket(1234);
        while(true) {
            Socket s = ss.accept();
            Thread t = new Thread(new Handler(s, m));
            t.start();
        }
    }
}
