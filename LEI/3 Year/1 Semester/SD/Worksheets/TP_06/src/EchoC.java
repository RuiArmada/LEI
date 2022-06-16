package TP_06.src;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class EchoC {

    public static void main(String[] args) {
        try {

            Socket socket = new Socket("localhost", 12345);

            BufferedReader input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter output = new PrintWriter(socket.getOutputStream());
            BufferedReader sysIn = new BufferedReader(new InputStreamReader(System.in));

            String userIn;

            while((userIn = sysIn.readLine()) != null) {
                output.println(userIn);
                output.flush();
                String res = input.readLine();
                System.out.println("Sum: " + res);
            }

            socket.shutdownOutput();
            
            String res = input.readLine();
            System.out.println("Average: " + res);

            socket.shutdownInput();
            socket.close();

        } catch(Exception e) {
            e.printStackTrace();
        }
    }
    
}
