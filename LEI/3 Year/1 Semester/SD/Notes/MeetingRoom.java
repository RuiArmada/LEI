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

public class MeetingRoom {

    public class Meeting {

        int idListaOcupante;
        Lock lockidListaOcupante;
        Condition aguardarDesocupacao;

        int nPessoasNaSala;
        Lock lockPessoasSala;

        Map<Integer,Integer> emEspera; // (id da Lista, numero de pessoas em espera)
        Lock lockWaitLine;
        
        /**
         * Class constructor
         */
        public Meeting() {
            this.idListaOcupante = 0;
            this.lockidListaOcupante = new ReentrantLock();
            this.aguardarDesocupacao = this.lockidListaOcupante.newCondition(); // é uma condição do lock da lista a Ocupar
            
            this.nPessoasNaSala = 0;
            this.lockPessoasSala = new ReentrantLock();

            this.emEspera = new HashMap<>();
            this.lockWaitLine = new ReentrantLock();
        }

        /**
         * The more people it has waiting the more priority it gets
         * @return id of the list with higher priority
         */
        int getPriority() {
            this.lockWaitLine.lock();
            try {
                int idLista = 0;
                int max = 0;
                for(Map.Entry<Integer,Integer> par : this.emEspera.entrySet()) {
                    int nPessoasWaiting = par.getValue();     
                    if (nPessoasWaiting > max) {
                        max = nPessoasWaiting;
                        idLista = par.getKey(); 
                    }
                }
                return idLista;
            } finally {
                this.lockWaitLine.unlock();
            }
        }

        /**
         * Blocks a member until he can enter the room
         * @param list id da Lista candidata
         * @throws InterruptedException
         */
        void participa(int list) throws InterruptedException { 
            this.lockWaitLine.lock();
            this.lockidListaOcupante.lock();

            //if it cannot enter then it gets added to the waiting line
            if(list != this.idListaOcupante) {
                if(this.emEspera.containsValue(list)) {
                    int n = this.emEspera.get(list);
                    this.emEspera.put(list, n+1);
                }else{
                    this.emEspera.put(list, 1);
                }
                this.lockWaitLine.unlock();
            }
            this.lockidListaOcupante.unlock(); 

            //If the room is clear, we need to assure that the person enters the list that has more people waiting
            this.lockidListaOcupante.lock();
            if(this.idListaOcupante == 0) {
                this.idListaOcupante = getPriority();
                if(this.idListaOcupante != 0) {
                    this.lockWaitLine.lock();
                    this.emEspera.remove(this.idListaOcupante);
                    this.lockWaitLine.unlock();
                    aguardarDesocupacao.signalAll();
                }
            }
            this.lockidListaOcupante.unlock();
            
            //Wait for turn
            while(list != idListaOcupante) aguardarDesocupacao.await();

            this.lockPessoasSala.lock();
            this.nPessoasNaSala ++;
            this.lockPessoasSala.unlock();
        }


        /**
         * The act of abandoning the meeting room
         * @param list id da Lista candidata
         */
        void abandona(int list) {
            lockPessoasSala.lock();
            this.nPessoasNaSala--;
            if(this.nPessoasNaSala == 0) {
                lockidListaOcupante.lock();
                this.idListaOcupante = getPriority();
                if(this.idListaOcupante != 0) {
                    this.emEspera.remove(this.idListaOcupante);
                    aguardarDesocupacao.signalAll();
                }
                lockidListaOcupante.unlock();
            }
            lockPessoasSala.unlock();
        }

        /**
         * Determines the number of elements inside the meeting room
         * @return Number of elements inside the meeting room
         */
        int nSala() {
            this.lockPessoasSala.lock();
            try {
                return this.nPessoasNaSala;
            } finally {
                this.lockPessoasSala.unlock();
            }
        }

        /**
         * Determines the number os people waiting to enter the meeting room
         * @return Number of people waiting
         */
        int aEspera() {
            this.lockPessoasSala.lock();
            try {
                int total = 0;
                for(Integer n : this.emEspera.values()) {
                    total += n;
                }
                return total;
            } finally {
                this.lockWaitLine.unlock();
            }
        }
    }

    class Handler implements Runnable {

        DataInputStream in;
        DataOutputStream ou;
        Meeting m;

        public Handler(Socket s, Meeting m) throws IOException {
            this.in = new DataInputStream(new BufferedInputStream(s.getInputStream()));
            this.ou = new DataOutputStream(new BufferedOutputStream(s.getOutputStream()));
            this.m = m;
        }

        public void run() {
            int nLista = in.readInt();

            Thread t = new Thread( () -> {
                
            boolean manterLigada = true;
            while(manterLigada) {
                try {
                    String comando = in.readUTF();
                        if (comando.equals("STATUS")) {
                            ou.writeChars("DENTRO: " + this.m.nSala() + " | FORA: " + this.m.aEspera());
                            ou.flush();
                        }

                    } catch (InterruptedException e) {
                        manterLigada = false;
                    }
                }    

            });
            t.start();

            m.participa(nLista);
            t.interrupt();
            ou.writeUTF("ENTRE");
            ou.flush();

            while (in.readUTF() != "ABANDONEI"); 
            m.abandona(nLista);
        }
    }

    class Server {

        public  void main(String[] args) throws IOException {
            
            ServerSocket ss = new ServerSocket(1234);
            Meeting m = new Meeting();
            try {
                while(true) {
                    Socket s = ss.accept();
                    Thread t = new Thread(new Handler(s,m));
                    t.start();
                }
            } catch (Exception ignored) {}
            ss.close();
        }
    }
    
}
