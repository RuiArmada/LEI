package UMAirlines.src;

import java.io.*;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * This class responsable for the handling of all the accounts present in the system.
 * This class makes a map, in our case, a Treemap where the key is the account's email address and the value is a User.
 */
public class Accounts implements Serializable{

    TreeMap<String,User> acc;
    public ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

    /**
     * Accounts Class Constructor
     */
    public Accounts() {
        this.acc = new TreeMap<>();
    }

    /**
     * Accounts Class Constructor
     * @param acc New TreeMap of Users
     */
    public Accounts(TreeMap<String,User> acc) {
        setAcc(acc);
    }

    /**
     * Accounts Class Constructor
     */
    public Accounts(Accounts acc) {
        this.acc = acc.getAcc();
    }

    /**
     * Makes a List with Accounts
     * @return List containing Accounts
     */
    public TreeMap<String,User> getAcc() {
        TreeMap<String,User> res = new TreeMap<>();
        for(Map.Entry<String,User> a : this.acc.entrySet())
            res.put(a.getKey(), a.getValue());
        return res;
    }

    /**
     * Makes a List with Accounts that are Admin Status
     * @param isAdmin boolean
     * @return List containing Admins
     */
    public TreeMap<String,User> getAdmins(boolean isAdmin) {
        TreeMap<String,User> res = new TreeMap<>();
        for(Map.Entry<String,User> a : this.acc.entrySet())
            if(a.getValue().isAdmin() == isAdmin)
                res.put(a.getKey(),a.getValue());
        return res;
    }

    /**
     * Changes the TreeMap
     * @param acc New TreeMap
     */
    public void setAcc(TreeMap<String,User> acc) {
        this.acc = new TreeMap<>();
        acc.entrySet().forEach( a -> { this.acc.put(a.getKey(), a.getValue()); });
    }

    /**
     * Adds a new User into the TreeMap
     * @param c User account to be added
     */
    void addAcc(User c) {
        this.acc.put(c.getEmail(), c);
    }

    void addAcc(String email, String name, String pass, boolean admin) {
        User u = new User(email, name, pass, admin);
        this.acc.put(email, u);
    }

    /**
     * Fetches a User account
     * @param mail User's email address
     * @return User
     */
    User getUser(String mail) {
        User c = this.acc.get(mail);
        return c;
    }

    /**
     * Fetches the Account's password 
     * @param email Account's email address
     * @return Account's password
     */
    String getPassword(String email) {
        String pass = null;
        User c = this.acc.get(email);
        if(c != null) pass = c.getPass();
        return pass;
    }

    /**
     * Checks if a Account exists via giving it's email address
     * @param mail Account's email address
     * @return <i>True</i> if the account exists. <i>False</i> if otherwise
     */
    boolean accountExists(String mail) {
        return this.acc.containsKey(mail);
    }

    /**
     * Conversion of the state of an object into a byte stream
     * @param filepath Path of the file
     * @throws IOException
     */
    public void serialise(String filepath)throws IOException{
        FileOutputStream fos = new FileOutputStream(filepath);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.close();
        fos.close();
    }

    /**
     * Conversion of a byte stream into an object
     * @param path Path of the file
     * @return New Accounts
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public static Accounts deserialize(String filepath) throws IOException,ClassNotFoundException{
        FileInputStream fis = new FileInputStream(filepath);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Accounts accounts = (Accounts) ois.readObject();
        ois.close();
        fis.close();
        return accounts;
    }
}
