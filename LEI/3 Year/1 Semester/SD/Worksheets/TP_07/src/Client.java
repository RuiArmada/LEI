package TP_07.src;

import java.io.*;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Client {
  
  public static Contact parseLine (String userInput) {
    String[] token = userInput.split(" ");
    if (token[3].equals("null"))
      token[3] = null;
    return new Contact (token[0],
                        Integer.parseInt(token[1]),
                        Long.parseLong(token[2]),
                        token[3],
                        new ArrayList<>(Arrays.
                                        asList(token).
                                        subList(4,token.length)));
  }

  public static void main(String[] args) throws IOException {
    Socket sock = new Socket("localhost",12345);
    BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
    DataOutputStream out = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));
    DataInputStream in = new DataInputStream(new BufferedInputStream(sock.getInputStream()));
    System.out.println("1) Add new contanct \n2) Show contact list\n\n0) Exit\n\nType wich option you desire:");
    String userInput;
    while ((userInput = stdin.readLine()) != null) {
      int opt = Integer.parseInt(userInput);
      out.writeInt(opt);
      out.flush();
      switch (opt) {
        case 1:
          userInput = stdin.readLine();
          Contact newc = parseLine(userInput);
          newc.serialize(out);
          out.flush();
          System.out.println(newc.toString());
          break;

        case 2:
          int n_c = in.readInt();
          List<Contact> contacts = new ArrayList<>(n_c);
          for (int i = 0 ; i < n_c ; i++) {
            contacts.add(Contact.deserialize(in));
          }
          for (Contact contact : contacts)
            System.out.println(contact.toString());
          break;

        default:
          sock.close();
          break;
      }               
    }
    sock.close();
  }

}
