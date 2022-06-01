package UMAirlines.src;

import java.io.*;
import java.util.Objects;

import UMAirlines.src.Exceptions.EmailDoesntExistException;
import UMAirlines.src.Exceptions.UserAlreadyAdminException;
import UMAirlines.src.Exceptions.UserIsntAdminException;

/**
 * This class is responsible for all the user information that is present in the system.
 * In our case a user is composed by an email, a username, the user's password and the admin flag that indicates if a user is an admin or not.
 */
public class User implements Serializable {

    private String email;
    private String name;
    private String password;
    private boolean isAdmin;

    /**
     * Class Constructor
     */
    public User() {
        this.email = null;
        this.name = null;
        this.password = null;
        this.isAdmin = false;
    }

    /**
     * Class Constructor
     * @param email User's email address
     * @param name User's name
     * @param password User's password
     * @param isAdmin User's admin flag
     */
    public User(String email, String name, String password, boolean isAdmin) {
        this.email = email;
        this.name = name;
        this.password = password;
        this.isAdmin = isAdmin;
    }

    /**
     * Class Constructor
     * @param c User
     */
    public User(User c) {
        this.email = c.getEmail();
        this.name = c.getName();
        this.password = c.getPass();
        this.isAdmin = c.isAdmin();
    }

    /**
     * Fetches the User's email address 
     * @return User's email address
     */
    public String getEmail() {
        return this.email;
    }

    /**
     * Fetches the User's name
     * @return User's name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Fetches the User's password
     * @return User's password
     */
    public String getPass() {
        return this.password;
    }

    /**
     * Fetches the User's admin flag
     * @return <i>True</i> if the User is an admin
     */
    public boolean isAdmin() {
        return this.isAdmin;
    }

    /**
     * Changes the email address
     * @param email New email
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Changes the name 
     * @param name New Name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Changes the password
     * @param password New password
     */
    public void setPass(String password) {
        this.password = password;
    }

    /**
     * Changes the admin flag
     * @param isAdmin New admin flag
     */
    public void setAdmin(boolean isAdmin) {
        this.isAdmin = isAdmin;
    }

    /**
     * Fetches the User's password using his email address
     * @param mail User's email address
     * @return User's password
     * @throws EmailDoesntExistException
     */
    public String getPass(String mail) throws EmailDoesntExistException {
        if(getEmail() == mail)
            return getPass();
        else
            throw new EmailDoesntExistException();
    }

    /**
     * Checks if a User exists via using the email address
     * @param email User's email address
     * @return <i>True</i> if the User exists. <i>False</i> if otherwise
     */
    public boolean exists(String email) {
        if(!(getEmail() == email))
            return false;
        else
            return true;
    }

    /**
     * Promotes the User to admin
     * @param email User's email address
     * @throws UserAlreadyAdminException
     */
    public void promote(String email) throws UserAlreadyAdminException {
        if(exists(email)) {
            if(isAdmin() != true)
                setAdmin(true);
            else
                throw new UserAlreadyAdminException();
        }
    }

    /**
     * Demotes the Amin to User
     * @param email Admin's email address
     * @throws UserIsntAdminException
     */
    public void demote(String email) throws UserIsntAdminException {
        if(exists(email)) {
            if(isAdmin() != false)
                setAdmin(false);
            else
                throw new UserIsntAdminException();
        }
    }

    /**
     * Compares an object to another
     * @param obj Object to be compared with
     * @return <i>True</i> if the objects are equal. <i>False</i> if otherwise
     */
    public boolean equals(Object obj) {
        if(obj == this)
            return true;
        if(obj == null || obj.getClass() != this.getClass())
            return false;
        User c = (User) obj;
        return Objects.equals(email, c.getEmail())
               &&
               Objects.equals(name, c.getName())
               &&
               Objects.equals(password, c.getPass())
               &&
               Objects.equals(isAdmin, c.isAdmin());
    }

    /**
     * Turns a Object into the string format
     * @return String
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Nome: ");
        sb.append(name);
        sb.append("\nPassword: ");
        sb.append(password);
        sb.append("\nAdmin: ");
        sb.append(isAdmin);
        return sb.toString();
    }

    /**
     * Conversion of the state of an object into a byte stream
     * @param path Path of the file
     * @throws IOException
     */
    public void serialize(String path) throws IOException {
        FileOutputStream output = new FileOutputStream(path);
        ObjectOutputStream obj = new ObjectOutputStream(output);
        obj.writeObject(this);
        obj.close();
        output.close();
    }

    /**
     * Conversion of a byte stream into an object
     * @param path Path of the file
     * @return New User
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public static User deserialize(String path) throws IOException, ClassNotFoundException {
        FileInputStream input = new FileInputStream(path);
        ObjectInputStream obj = new ObjectInputStream(input);
        User acc = (User) obj.readObject();
        obj.close();
        input.close();
        return acc;
    }
    
}
