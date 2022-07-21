import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Voting {
 
    private Set<Integer> userChecked = new HashSet<>();
    private Lock userCheckedLock = new ReentrantLock();

    private boolean voteOpen = true;
    private Lock voteStatusLock = new ReentrantLock();

    private Set<Integer> cabinFull = new HashSet<>();
    private Lock cabinFullLock = new ReentrantLock();
    
    private Queue<Integer> cabinFree = new ArrayDeque<>();
    private Lock cabinFreeLock = new ReentrantLock();
    private Condition waitForCabin = cabinFreeLock.newCondition();

    private Map<Integer,Integer> votes = new HashMap<>(); // (idLista,nVotos)
    private Lock voteLock = new ReentrantLock();

    private Integer nVotes = 0;
    private Lock nVotesLock = new ReentrantLock();
    private Condition waitForAllToVote = nVotesLock.newCondition();

    boolean verifica(int uId) {
        boolean status = false;
        try { // Verifica qual é o status da Votação
            this.voteStatusLock.lock();
            status = this.voteOpen;
        } finally {
            this.voteStatusLock.unlock();
        }
        if(status) { // Se a votação estiver aberta
            try { // Verifica se o uID que é fornecido está presente no SET de Users já verificados
                userCheckedLock.lock();
                boolean isChecked = userChecked.contains(uId); // True - if the Set contains it | False - if otherwise
                if(isChecked) return false; // Se o uID já estiver verificado a função retorna False
                else { // Se o user não estiver verificado ele é adicionado ao SET e o numero de votos efetuados aumenta em 1
                    try {
                        nVotesLock.lock();
                        nVotes ++;
                    } finally {
                        nVotesLock.unlock();
                    }
                    userChecked.add(uId);
                    return true;
                }
            } finally {
                userCheckedLock.unlock();
            }
        } else return false; // Se a votação estiver fechada retorna False
    }

    int esperaPorCabine() throws InterruptedException {
        int ocupy;
        try {
            cabinFreeLock.lock();
            while(cabinFree.isEmpty()) // Se o Set estiver vazio sinalizo a thread para esperar
                waitForCabin.await();
            ocupy = cabinFree.poll(); // Se o Set tiver membros eu vou tirar o id da primeira cabina e igualar a minha var de resultado
        } finally {
            cabinFreeLock.unlock();
        } try {
            cabinFullLock.lock();
            cabinFull.add(ocupy); // Adiciona a Cabina que acabou de ser ocupada ao Set de cabinas ocupadas
        } finally {
            cabinFullLock.unlock();
        }
        return ocupy; // Retorna o Id da cabina ocupada
    }

    void vota(int choice) {
        int nVotes;
        try {
            voteLock.lock();
            if(votes.containsKey(choice)) {
                nVotes = votes.get(choice); // Vai buscar o nVotos da Lista desejada
                votes.put(choice,nVotes+1);
            } else votes.put(choice, 1);
        } finally {
            voteLock.unlock();
        }
    }

    void desocupaCabine(int idC) throws InterruptedException {
        boolean frees = true;
        try {
            cabinFullLock.lock();
            if(cabinFull.contains(idC)) cabinFull.remove(idC);
            else frees = false;
        } finally {
            cabinFullLock.unlock();
        }
        if(frees) {
            try{
                cabinFreeLock.lock();  
                cabinFree.add(idC); // Adiciona a cabina que vai ser desocupada à lista de cabinas desocupadas
                waitForCabin.signalAll(); 
            } finally {
                cabinFreeLock.unlock();
            } try {
                nVotesLock.lock();
                nVotes --; // Decrementa o numero de pessoas que se encontram atualmente dentro de uma cabine a votar
                waitForAllToVote.signalAll();
            } finally {
                nVotesLock.unlock();
            }
            
        }
    } 

    int vencedor() throws InterruptedException {
        int candidateVotes;
        int winner = -1;
        int maxVotes = 0;
        try {
            voteStatusLock.lock();
            voteOpen = false;
            nVotesLock.lock();
            try {
                while(nVotes != 0) waitForAllToVote.await();
            } finally {
                nVotesLock.unlock();
            } try {
                voteLock.lock();
                for(Map.Entry<Integer,Integer> candidate : this.votes.entrySet()) {
                    candidateVotes = candidate.getValue();
                    if(candidateVotes > maxVotes) {
                        winner = candidate.getKey();
                        maxVotes = candidateVotes;
                    }
                }
                return winner;
            } finally {
                voteLock.unlock();
            }
        } finally {
            voteStatusLock.unlock();
        }
    }
}

class Server {

    public static void main(String[] args) throws IOException {
        Voting v = new Voting();
        ServerSocket ss = new ServerSocket(1234);
        while(true) {
            Socket s = ss.accept();
            DataInputStream in = new DataInputStream(new BufferedInputStream(s.getInputStream()));
            DataOutputStream ou = new DataOutputStream(new BufferedOutputStream(s.getOutputStream()));
            Runnable worker = () -> {
                try {
                    int id = in.readInt();
                    if(v.verifica(id)) {
                        int nCabin = v.esperaPorCabine();
                        ou.writeUTF("VOTE NA CABINE " + nCabin);
                        ou.flush();
                        int voto = in.readInt();
                        v.vota(voto);
                        ou.writeUTF("VOTO CONFIRMADO");
                        ou.flush();
                        v.desocupaCabine(nCabin);
                    } else {
                        ou.writeUTF("INVALIDO");
                        ou.flush();
                    }
                } catch(Exception ignored) {}
            };
            Thread t = new Thread(worker);
            t.start();
        }
    }
}