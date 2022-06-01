package Model;

import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;

public class User_cat implements Serializable{
    private static final long serialVersionUID = -2782540373809491375L ;

    private Map<String,User> u_catalog;
    private int n_users;

/**
	 * Empty Constructor for the User_cat Class
	 */
    public User_cat(){
        this.u_catalog = new TreeMap<>();
        this.n_users = 0;
    }
 /**
     * Constructor for the User_cat Class
     * @param uCat - User_cat
     */
    public User_cat(User_cat uCat){
        setU_catalog(uCat.getU_catalog());
        this.n_users = uCat.getN_users();
    }
/**
     * Constructor for the User_cat Class
     * @param u_catalog - Map
     * @param n_users - Int
     */
    public User_cat(Map<String,User> u_catalog, int n_users){
        setU_catalog(u_catalog);
        this.n_users = n_users;
    }
/**
	 * Transforms the object to a String
	 * @return String
	 */
    public String toString() {
        StringBuilder s = new StringBuilder();
        for (String id : this.u_catalog.keySet()) {
            s.append("\nUser ID: ");
            s.append(id);
            s.append("\n");
            s.append(this.u_catalog.get(id).toString());
        }
        s.append("\nTotal number of Users: ");
        s.append(this.n_users);
        return super.toString();
    }
 /**
	 * Clones the object User_cat
	 * @return User_cat
	 */
    public User_cat clone() {
        return new User_cat(this);
    }
 /**
	 * Method that compares one Object to another
	 * @return Boolean
	 */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        User_cat u1 = (User_cat) obj;
        return u1.getN_users() == this.n_users &&
               u1.u_catalog.equals(this.u_catalog);
    }
    

    //----------------------------- gets e sets ------------------------------------------
 /**
     * Allows access to said variable
     * @return n_users - Int
     */
    public int getN_users() {
        return this.n_users;
    }
/**
     * Allows access to said variable
     * @return u_catalog - Map
     */
    public Map<String, User> getU_catalog() {
        return this.u_catalog;
    }

 /**
     * Allows the change of said variable
     * @param n_users - Int
     */
    public void setN_users(int n_users) {
        this.n_users = n_users;
    }
/**
     * Allows the change of said variable
     * @param u_catalog - Map
     */
    public void setU_catalog(Map<String, User> u_catalog) {
        u_catalog.entrySet().forEach(u -> { this.u_catalog.put(u.getKey(), u.getValue().clone());});
    }
 /**
     * Method that inserts a User to the Catalog
     * @param id - String
     * @param user - User
     */
    public void put(String id, User user){
	    this.u_catalog.put(id, user);
    }
 /**
     * Method that Increments the n_users
     */
    public void increment_n(){
	    this.n_users++;
    }
     /**
     * Method that checks if the Catalog contains a ID
     * @param id - String
     * @return Boolean
     */
    public boolean contains(String id){
        return this.u_catalog.containsKey(id);
    }

}
