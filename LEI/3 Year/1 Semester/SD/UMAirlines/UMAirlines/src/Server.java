package UMAirlines.src;

import java.io.File;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.time.LocalDate;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;
/**
 * Class that cointains all the information in the system stored.
 * This class is also responsible for the logical part of the application.
 * The server is responsible handling the queries done by the client.
 */
public class Server {
    /**
     * Main class that starts the Server
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception{
        ServerSocket ss = new ServerSocket(12345);

        final Accounts accounts;
        final Flights flights;
        final Reservations reservations;

        File f = new File("accounts.um");
        if (!f.exists())
            accounts = new Accounts();
        else
            accounts = Accounts.deserialize("accounts.um");

        f = new File("flights.um");
        if(!f.exists())
            flights = new Flights();
        else
            flights = Flights.deserialize("flights.um");

        f = new File("reservas.um");
        if(!f.exists())
            reservations = new Reservations();
        else
            reservations = Reservations.deserialize("reservas.um");

        ReentrantLock liuLock = new ReentrantLock();
        HashSet<String> loogedInUsers = new HashSet<>();

        while(true) {
            Socket s = ss.accept();
            Connect c = new Connect(s);

            Runnable runServer = () -> {
                try (c) {
                    while (true) {
                        boolean loogedIn = false;
                        Frames frames = c.recieve();

                        if (frames.tag == 0) { 
                            System.out.println("Log-in attempt.");
                            String email = frames.name;
                            String pass = new String(frames.data);
                            String expectedPass;
                            boolean isAdmin = false;
                            accounts.lock.readLock().lock();
                            try {
                                expectedPass = accounts.getPassword(email);
                                if (expectedPass != null) isAdmin = accounts.getUser(email).isAdmin();
                            } finally {
                                accounts.lock.readLock().unlock();
                            }

                            if (expectedPass != null) {
                                if (expectedPass.equals(pass)) {
                                    if(isAdmin) c.send_f_Info(0,"","Sessão Admin iniciada com Sucesso!".getBytes());
                                    else c.send_f_Info(0,"","Sessão Cliente iniciada com Sucesso!".getBytes());
                                    loogedIn = true;
                                    liuLock.lock();
                                    try{loogedInUsers.add(frames.name);}
                                    finally {liuLock.unlock();}
                                } else {
                                    c.send_f_Info(0, "", "Erro: Password incorreta".getBytes());
                                }
                            } else {
                                c.send_f_Info(0, "", "Erro: Conta não existe".getBytes());
                            }

                        } else if (frames.tag == 1) {
                            System.out.println("Tentativa de registo.");
                            String email = frames.name;
                            String[] info = new String(frames.data).split("-");
                            String pass = info[0];
                            String name = info[1];
                            System.out.println("novo registo de: " + name);
                            boolean isAdmin = info[2].equals("true");
                            accounts.lock.writeLock().lock();
                            try {
                                if (accounts.accountExists(email)) {
                                    c.send_f_Info(1, "", "Erro: Conta já existe".getBytes());
                                } else {
                                    accounts.addAcc(email, name, pass, isAdmin);
                                    accounts.serialise("accounts.um");
                                    c.send_f_Info(frames.tag, "", "Conta criada com sucesso".getBytes());
                                    loogedIn = true;
                                    liuLock.lock();
                                    try {loogedInUsers.add(frames.name);}
                                    finally {liuLock.unlock();}
                                }
                            } finally {
                                accounts.lock.writeLock().unlock();
                            }

                        } else if (frames.tag == 2) {
                            System.out.println("Tentativa de registo de voo.");
                            String[] info = new String(frames.data).split("-");
                            String start = info[0];
                            String end = info[1];
                            String cap = info[2];
                            Integer capacity = Integer.parseInt(cap);
                            boolean success = false;
                            flights.lock.writeLock().lock();
                            try {
                                if (!flights.flightExists(start, end, capacity)) {
                                    flights.addFlight(start, end, capacity);
                                    flights.serialize("flights.um");
                                    success = true;
                                }
                            } finally {
                                flights.lock.writeLock().unlock();
                            }
                            if(success) {
                                System.out.println("Voo registado!");
                                c.send_f_Info(2, "", "Voo adicionado com sucesso!!".getBytes());
                            }
                            else c.send_f_Info(2, "", "Erro: Voo já existe".getBytes());
                        }

                        else if(frames.tag == 3) {
                            System.out.println("Fechar data.");
                            String date = new String(frames.data);
                            LocalDate closeDate = LocalDate.parse(date);
                            reservations.lock.writeLock().lock();
                            try {
                                reservations.addClosed(closeDate);
                                reservations.serialize("reservas.um");
                            } finally {
                                reservations.lock.writeLock().unlock();
                            }
                            c.send_f_Info(3, "", "Data fechada com sucesso".getBytes());
                        }

                        else if(frames.tag == 4) {
                            Boolean success = false;
                            int wich = 0;
                            System.out.println("A reservar voo sem escala");
                            String[] info = new String(frames.data).split(" ");
                            String origem = info[0];
                            String destino = info[1];
                            String data = info[2];
                            LocalDate date = LocalDate.parse(data);
                            Integer mid = null;
                            Integer vooUm;
                            flights.lock.readLock().lock();
                            try {
                                vooUm = flights.getFlightId(origem,destino);
                            } finally {
                                flights.lock.readLock().unlock();
                            }

                            if(vooUm == null){
                                wich = 1;
                            }
                            else{
                                List<Integer> id = new ArrayList<>();
                                reservations.lock.writeLock().lock();
                                try {
                                    if(reservations.verifyCapacity(vooUm,date,flights.getFlight(vooUm).getCapacity())){
                                        if(reservations.dateNotClosed(date)) {
                                            id.add(vooUm);
                                            Reservation r = new Reservation(frames.name, id, date);
                                            mid = reservations.idMaker.getAndIncrement();
                                            reservations.addRes(r,mid);
                                            reservations.serialize("reservas.um");
                                            success = true;
                                        }
                                        else wich = 4;
                                    }
                                    else wich = 6;

                                } finally{
                                    reservations.lock.writeLock().unlock();
                                }
                            }
                            if(success) {
                                String output = "Reserva com o id " + Integer.toString(mid) + " registada com sucesso!!\n";
                                c.send_f_Info(4, "", output.getBytes());
                            }
                            else{
                                if(wich!=0){
                                    if(wich == 1){
                                        c.send_f_Info(4, "", "Erro: Um voo Origem->Destino não existe".getBytes());
                                    }
                                    else if(wich == 4){
                                        c.send_f_Info(4, "", "Erro: A data em que pretende reservar já se encontra fechada!".getBytes());
                                    }
                                    else if(wich == 6){
                                        c.send_f_Info(4, "", "Erro: O voo Origem->Destino está cheio no dia selecionado".getBytes());
                                    }
                                }
                                else c.send_f_Info(4, "", "Erro: Reserva já existe".getBytes());
                            }
                        }

                        else if(frames.tag == 5) {
                            int x = 0;
                            Boolean success = false;
                            int wich = 0;
                            System.out.println("A reservar voo com escala");
                            String[] info = new String(frames.data).split("/");
                            Integer n = Integer.parseInt(info[0]);
                            String origem = info[1];
                            String[] escala = info[2].split(",");
                            System.out.println(escala[0]);
                            String destino = info[3];
                            String data = info[4];
                            Integer mid = null;
                            LocalDate date = LocalDate.parse(data);
                            flights.lock.readLock().lock();
                            try {
                                Integer vooUm = flights.getFlightId(origem, escala[0]);
                                Integer vooDois = flights.getFlightId(escala[n-1],destino);
                                if(vooUm == null || vooDois == null){
                                    if(vooUm == null) wich = 1;
                                    else wich = 2;
                                }
                                else{
                                    reservations.lock.writeLock().lock();
                                    try {
                                        if(reservations.dateNotClosed(date)) {
                                            List<Integer> id = new ArrayList<>();
                                            if(reservations.verifyCapacity(vooUm,date,flights.getFlight(vooUm).getCapacity())){
                                                id.add(vooUm);
                                                while(x<n-1 && x>=0){
                                                    Integer voo = flights.getFlightId(escala[x], escala[x+1]);
                                                    x = x+1;
                                                    if(voo == null){
                                                        x = -1;
                                                    } else if(reservations.verifyCapacity(voo,date,flights.getFlight(voo).getCapacity()))
                                                        id.add(voo);
                                                    else{
                                                        x = -1;
                                                        wich = 5;
                                                    }
                                                }
                                                if (x != -1){
                                                    if(reservations.verifyCapacity(vooDois,date,flights.getFlight(vooDois).getCapacity())){
                                                        id.add(vooDois);
                                                        Reservation r = new Reservation(frames.name, id, date);
                                                        mid = reservations.idMaker.getAndIncrement();
                                                        reservations.addRes(r,mid);
                                                        reservations.serialize("reservas.um");
                                                        success = true;
                                                    }
                                                    else{
                                                        wich = 7;
                                                    }
                                                }
                                            }
                                            else{
                                                wich = 6;
                                            }
                                        }else wich = 40;

                                    } finally {
                                        reservations.lock.writeLock().unlock();
                                    }
                                }
                            }
                            finally {
                                flights.lock.readLock().unlock();
                            }
                            if(success) {
                                String output = "Reserva com o id " + Integer.toString(mid) + " registada com sucesso!!";
                                c.send_f_Info(5, "", output.getBytes());
                            }
                            else{
                                if(wich!=0 || x == -1){
                                    if(wich == 1){
                                        c.send_f_Info(5, "", "Erro: Um voo Origem->Escala não existe".getBytes());    
                                    }
                                    if(x == -1){
                                        c.send_f_Info(5, "", "Erro: Um voo Escala->Escala não existe".getBytes());    
                                    }
                                    if(wich == 2){
                                        c.send_f_Info(5, "", "Erro: Um voo Escala->Destino não existe".getBytes());
                                    }
                                    if(wich == 6){
                                        c.send_f_Info(5, "", "Erro: O voo Origem->X não existe no dia selecionado".getBytes());
                                    }
                                    if(wich == 5){
                                        c.send_f_Info(5, "", "Erro: O voo Escala->X não existe no dia selecionado".getBytes());
                                    }
                                    if(wich == 7){
                                        c.send_f_Info(5, "", "Erro: O voo X->Destino não existe no dia selecionado".getBytes());
                                    }
                                    if(wich == 40) c.send_f_Info(5, "", "Erro: A data já foi fechada!!".getBytes());
                                }
                                else c.send_f_Info(5, "", "Erro: Reserva já existe".getBytes());
                            }
                        }

                        else if(frames.tag == 10) {
                            Integer wh = null;
                            System.out.println("Camcelar uma reserva: ");
                            String user = frames.name;
                            String res = new String(frames.data);
                            Integer id = Integer.parseInt(res);
                            reservations.lock.writeLock().lock();
                            try {
                                if(reservations.reserVationExists(id)) {
                                    if(reservations.fromUser(id, user)) {
                                        LocalDate dateR = reservations.getReservations().get(id).getFlightDate();
                                        if(reservations.dateNotClosed(dateR)) {
                                            reservations.removeReservation(id);
                                            reservations.serialize("reservas.um");
                                            wh = 1;
                                        }
                                        else wh = 0;
                                    }
                                    else wh = -2;
                                }
                                else wh = -1;
                            }finally {
                                reservations.lock.writeLock().unlock();
                            }

                            if(wh == 1) {
                                c.send_f_Info(10, "", "Reserva cancelada com sucesso!!".getBytes());
                            }
                            else if(wh == -1) c.send_f_Info(10, "", "A reserva que pretende cancelar não existe".getBytes());
                            else if(wh == 0) c.send_f_Info(10, "", "A data da reserva já se encontra fechada".getBytes());
                            else c.send_f_Info(10, "", "Não pode cancelar uma reserva que não é sua".getBytes());
                        }

                        else if(frames.tag == 15) {

                            TreeMap<Integer, Flight> flightlist;
                            flights.lock.readLock().lock();
                            try {
                                flightlist = flights.getFlight();
                            } finally {
                                flights.lock.readLock().unlock();
                            }

                            StringBuilder sb= new StringBuilder();
                            for(Map.Entry<Integer, Flight> flig : flightlist.entrySet()) {
                                Flight fl = flig.getValue();
                                String st = fl.getStartLocation();
                                String en = fl.getEndLocation();
                                sb.append(String.format("Origem: %s ---> Destino: %s\n", st, en));
                            }
                            c.send_f_Info(15, "", sb.toString().getBytes());
                        }
                        
                        else if(frames.tag == 99) {
                            liuLock.lock();
                            try{loogedInUsers.remove(frames.name);}
                            finally {liuLock.unlock();}
                        }
                    }
                } catch (IOException ignored) {
                }
            };

            new Thread(runServer).start();
        }
    }
}
