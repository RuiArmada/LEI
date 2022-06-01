package UMAirlines.src;

import java.io.*;
import java.net.Socket;
import java.time.LocalDate;

import static UMAirlines.src.GUI.LogoUI.Logo;
/**
 * Class that handles all the interaction of the User with the system.
 * In this class we handle all the user input, format it and send it to the Server.
 */
public class Client {

    /**
     * Main class that starts the Client
     * @param args
     * @throws IOException
     * @throws InterruptedException
    */
    public static void main(String[] args) throws IOException, InterruptedException {
        Socket socket = new Socket("localhost", 12345);
        Demultiplexer dm = new Demultiplexer(new Connect(socket));
        BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

        dm.start();

        String user = null;
        boolean isAdmin = false;

        while (user == null) {
            Logo();
            System.out.println("\033[1;31m" + "************** UMAirlines ****************");
            System.out.println("1) Iniciar sessão.\n" + "2) Registar nova conta");
            System.out.println("\033[1;31m" + "******************************************" + "\033[0m");
            System.out.println("\nOpção: ");
            String opt = stdin.readLine();
            if (opt.equals("1")) { 
                System.out.println("\033[1;31m" + "************** Iniciar Sessão ****************");
                System.out.println("Introduza o seu email: ");
                String email = stdin.readLine();
                System.out.println("Introduza a sua palavra-passe:");
                String pass = stdin.readLine();
                dm.send(0, email, pass.getBytes());
                String resp = new String(dm.recieve(0));

                if (!resp.startsWith("Error:")) {
                    if (resp.contains("Admin")) isAdmin = true;
                    user = email;
                }
                System.out.println(resp);
            } else if (opt.equals("2")) { 
                System.out.println("\033[1;33m" + "************** Registar Conta ****************");
                System.out.println("Introduza o seu endereço de email: ");
                String email = stdin.readLine();
                System.out.println("Introduza a sua palavra-passe: ");
                String pass = stdin.readLine();
                System.out.println("Introduza o seu nome: ");
                String name = stdin.readLine();
                System.out.println("É admin? ");
                if (stdin.readLine().equalsIgnoreCase("Y")) isAdmin = true;
                dm.send(1, email, String.format("%s-%s-%s", pass, name, isAdmin).getBytes());
                String resp = new String(dm.recieve(1));

                if (!resp.startsWith("Error")) {
                    user = email;
                }
                System.out.println(resp);
            }
        }

        boolean exit = false;
        while (!exit) {
            if (isAdmin) {
                System.out.println("\033[1;33m" + "************** Admin Options ****************");
                System.out.println("1) Registar Voo.\n"
                        + "2) Consultar Lista de Voos\n"
                        + "3) Encerrar o dia\n"
                        + "0) Sair");
                System.out.println("\033[1;33m" + "******************************************" + "\033[0m");
                System.out.println("Opção: ");
                String opt = stdin.readLine();
                switch (opt) {
                    case "0":
                        dm.send(99, user, "".getBytes());
                        exit = true;
                        System.out.println("\nO sistema será encerrado agora.");
                        System.out.println("\033[1;36m" + "Sessão Terminada!" + "\033[0m");
                        break;
                    case "1":
                        System.out.println("\033[1;33m" + "************** Registar Voo ****************");
                        System.out.println("Introduza a origem do voo: ");
                        String start = stdin.readLine();
                        System.out.println("Introduza o destino do voo: ");
                        String dest = stdin.readLine();
                        System.out.println("Introduza a capacidade do voo:");
                        String capacity = stdin.readLine();
                        dm.send(2, user, String.format("%s-%s-%s", start, dest, capacity).getBytes());
                        String resp = new String(dm.recieve(2));
                        System.out.println(resp);
                        break;
                    case "2":
                        System.out.println("\033[1;33m" + "************** Lista de Voos ****************");
                        dm.send(15, user, "".getBytes());
                        String repl = new String(dm.recieve(15));
                        System.out.println(repl);
                        break;
                    case "3":
                        System.out.println("\033[1;33m" + "************** Encerrar o dia ****************");
                        System.out.println("Insira o dia que quer encerrar no formato aaaa-mm-dd: ");
                        String date = stdin.readLine();
                        dm.send(3, user, date.getBytes());
                        System.out.println("Todos os sistema de reservas sera colocadas em pausa!!");
                        String rep = new String(dm.recieve(3));
                        System.out.println(rep);
                        break;
                }
            } else {
                System.out.println("\033[1;33m" + "************** Bem-Vindo a UMAirlines ****************");
                System.out.println(
                                "1) Reservar um voo.\n"
                                + "2) Consultar Lista de Voos\n"
                                + "3) Cancelar Reserva\n"
                                + "0) Sair");
                System.out.println("\033[1;33m" + "******************************************" + "\033[0m");
                System.out.println("Opção: ");
                String opt2 = stdin.readLine();
                switch (opt2) {
                    case "0":
                        dm.send(99, user, "".getBytes());
                        exit = true;
                        System.out.println("\nO sistema será encerrado agora.");
                        System.out.println("\033[1;36m" + "Sessão Terminada!" + "\033[0m");
                        break;
                    case "1":
                        String rep = new String();
                        System.out.println("\033[1;33m" + "************** Reservar Voo ****************");
                        System.out.println("Insira a Origem:");
                        String origem1 = stdin.readLine();
                        System.out.println("Insira o Destino");
                        String destino1 = stdin.readLine();
                        System.out.println("Insira uma Data (aaaa-mm-dd)");
                        String data1 = stdin.readLine();
                        LocalDate d1 = LocalDate.parse(data1);
                        String d2 = d1.toString();
                        System.out.println("Deseja indicar uma escala? Y/N");
                        String op = stdin.readLine();
                        switch (op) {
                            case "n": 
                            case "N":
                                dm.send(4, user, String.format("%s %s %s", origem1, destino1, d2).getBytes());
                                rep = new String(dm.recieve(4));
                                break;

                            case "y": 
                            case "Y":
                                System.out.println("Insira quantas a escalas");
                                Integer n = Integer.parseInt(stdin.readLine());
                                String escalas = new String();
                                int x = 0;
                                while (x < n) {
                                    System.out.println("insira a escala " + x+1);
                                    String escala = stdin.readLine();
                                    escalas = escalas + escala;
                                    x = x + 1;
                                    if(x<n) escalas = escala + ",";
                                }
                                dm.send(5, user, String.format("%d/%s/%s/%s/%s", n, origem1, escalas, destino1, d2).getBytes());
                                rep = new String(dm.recieve(5));
                                break;
                        }
                        System.out.println(rep);
                        break;
                    case "2":
                        System.out.println("\033[1;33m" + "************** Lista de Voos ****************");
                        dm.send(15, user, "".getBytes());
                        String repl = new String(dm.recieve(15));
                        System.out.println(repl);

                        break;
                    case "3":
                        System.out.println("\033[1;33m" + "************** Cancelar Reserva ****************");
                        System.out.println("\nInsira o id da reserva que pretende cancelar: ");
                        String res = stdin.readLine();
                        dm.send(10, user, res.getBytes());
                        String resp = new String(dm.recieve(10));
                        System.out.println(resp);
                        break;
                }
            }
        }
        dm.close();
    }
}
