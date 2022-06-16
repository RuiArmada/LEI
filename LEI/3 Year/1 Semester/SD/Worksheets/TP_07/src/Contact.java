package TP_07.src;

import java.io.*;
import java.util.*;

class Contact implements Serializable  {
  
  private String name;
  private int age;
  private long phone;
  private String company;
  private List<String> emails;

  public Contact (String name, int age, long phone, String company, List<String> emails) {
    this.name = name;
    this.age = age;
    this.phone = phone;
    this.company = company;
    this.emails = new ArrayList<>(emails);
  }

  public String toString() {
    StringBuilder builder = new StringBuilder("{");
    builder.append(this.name).append(";");
    builder.append(this.age).append(";");
    builder.append(this.phone).append(";");
    builder.append(this.company).append(";");
    builder.append("{");
    for (String s : this.emails) {
      builder.append(s).append(";");
    }
    builder.append("}");
    return builder.toString();
  }

  public void serialize(DataOutputStream out) throws IOException {
    out.writeUTF(this.name);
    out.writeInt(this.age);
    out.writeLong(this.phone);
    out.writeBoolean(this.company != null);
    if (this.company != null)
      out.writeUTF(this.company);
    out.writeInt(this.emails.toArray().length);
    for (String email : emails)
      out.writeUTF(email);
  }

  public static Contact deserialize(DataInputStream in) throws IOException {
    String name = in.readUTF();
    int age = in.readInt();
    long phone = in.readLong();
    boolean hasComp = in.readBoolean();
    String company = null;
    if (hasComp)
      company = in.readUTF();
    int n_Mail = in.readInt();
    List<String> emails = new ArrayList<>();
    for (int i = 0 ; i < n_Mail; i++) {
      emails.add(in.readUTF());
    }
    return new Contact(name,age,phone,company,emails);
  }

}
