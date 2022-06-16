package TP_06.src;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

public class ServerWorker  implements Runnable{

    private Socket sock;

    public ServerWorker(Socket sock) {
        this.sock = sock;
    }

    public void run() {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
            PrintWriter ou = new PrintWriter(sock.getOutputStream());
            String l;
            List<Integer> num = new ArrayList<>();

            while((l = in.readLine()) != null) {
                num.add(Integer.parseInt(l));
                ou.println(num.stream().reduce(0, Integer::sum));
                ou.flush();
            }
            ou.println(num.stream().reduce(0, Integer::sum) / num.size());
            ou.flush();
            sock.shutdownOutput();
            sock.shutdownInput();
            sock.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
