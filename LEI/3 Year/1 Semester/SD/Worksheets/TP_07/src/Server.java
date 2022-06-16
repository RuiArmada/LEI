package TP_07.src;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class ContactList {

  private List<Contact> contacts;

  public ContactList() {
    contacts = new ArrayList<>();

    contacts.add(new Contact("Eren Yeager", 20, 253123123, null, new ArrayList<>(Arrays.asList("tatakae@gmail.com"))));
    contacts.add(new Contact("Bakugou Katsuki", 10, 961123321, "Best Jeanist", new ArrayList<>(Arrays.asList("dammed_nerd@hotmail.com"))));
    contacts.add(new Contact("Izuko Midoriya", 5, 932435586, "Sir Nighteye", new ArrayList<>(Arrays.asList("deku@gmail.com"))));
  }

  public boolean addContact(Contact contact) throws IOException {
    contacts.add(contact);
    return true;
  }

  public void getContact(DataOutputStream out) throws IOException {
    System.out.println("B");
    out.writeInt(contacts.size());
    System.out.println("C");
    for (Contact contact : contacts) {
      contact.serialize(out);
      out.flush();
    }
  }

}

class ServerWork implements Runnable {

  private Socket sock;
  private ContactList contactList;

  public ServerWork(Socket s, ContactList cl) {
    this.contactList = cl;
    this.sock = s;
  }

  @Override
  public void run() {
    try {
      DataInputStream in = new DataInputStream(new BufferedInputStream(sock.getInputStream()));
      DataOutputStream out = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));
      while (true) {
        int opt = in.readInt();
        switch (opt) {
          case 1:
            Contact newC = Contact.deserialize(in);
            contactList.addContact(newC);
            System.out.println(newC.toString());
            break;

          case 2:
            contactList.getContact(out);
            break;

          case 0:
            throw new EOFException();

          default:
            break;
        }
      }
    } catch (EOFException e) {
      System.out.println("Connection ended");
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

}

public class Server {
  
  public static void main(String[] args) throws IOException {
    ServerSocket ss = new ServerSocket(12345);
    ContactList cl = new ContactList();
    while (true) {
      Socket s = ss.accept();
      Thread w = new Thread(new ServerWork(s,cl));
      w.start();
    }
  }

}
