package Model;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

public class User implements Serializable{
    private static final long serialVersionUID = -6154155925408435289L;

    private String user_name;
    private Set<String> friends;
    private int n_reviews;
/**
	 * Empty Constructor for the User Class
	 */
    public User(){
        this.user_name = "undefined";
        this.friends = new HashSet<String>();
        this.n_reviews = 0;
    }
 /**
     * Constructor for the User Class
     * @param user_name - String
     * @param friends - Set
     * @param n_reviews - Int
     */
    public User(String user_name, Set<String> friends, int n_reviews){
        this.user_name = user_name;
        setFriends(friends);
        this.n_reviews = n_reviews;
    }
/** 
     * Constructor for the User Class 
     * @param user_name - String
     * @param n_reviews - Int
     */
    public User(String user_name, int n_reviews){
        this.user_name = user_name;
        this.friends = new HashSet<>();
        this.n_reviews = n_reviews;
    }
  /**
     * Constructor for User Class
     * @param user - User
     */
    public User(User user){
        this.user_name = user.getUser_name();
        setFriends(user.getFriends());
        this.n_reviews = user.getN_reviews();
    }
 /**
	 * Transforms the object to a String
	 * @return String
	 */
    public String toString() {
        int n = 0;
        StringBuilder s = new StringBuilder();
        s.append("User Name: ");
        s.append(this.user_name);
        s.append("\nNumber of Reviews: ");
        s.append(this.n_reviews);
        if(this.friends.size()!=0){
            s.append("\nFriends: ");
            for (String idf : this.friends) {
                s.append(idf);
                n++;
                if(n>5){
                    n = 0;
                    s.append(",\n");
                } else {
                    s.append(", ");
                }
            }     
        }
        return super.toString();
    }
 /**
	 * Clones the object User
	 * @return User
	 */
    public User clone() {
        return new User(this);
    }
     /**
	 * Method that compares one Object to another
	 * @return Boolean
	 */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        User u1 = (User) obj;
        return u1.getUser_name().equals(this.user_name) &&
               u1.getN_reviews() == this.n_reviews &&
               u1.getFriends().equals(this.friends);
    }

    //---------------------------- gets & sets -------------------------------------------

    /**
    * Allows access to said variable
    * @return user_name - String
    */
    public String getUser_name() {
        return this.user_name;
    }
    /**
     * Allows access to said variable
     * @return friends - Set
     */
    public Set<String> getFriends() {
        return this.friends;
    }
    /**
     * Allows access to said variable
     * @return n_reviews - Int
     */
    public int getN_reviews() {
        return this.n_reviews;
    }
/**
     * Allows the change of said variable
     * @param n_reviews - Int
     */
    public void setN_reviews(int n_reviews) {
        this.n_reviews = n_reviews;
    }
 /**
     * Allows the change of said variable
     * @param user_name - String
     */
    public void setUser_name(String user_name) {
        this.user_name = user_name;
    }
 /**
     * Method that increments the number of reviews
     */
    public void plusRev(){
        this.n_reviews++;
    }
 /**
     * Allows the change of said variable
     * @param friends - Set
     */
    public void setFriends(Set<String> friends) {
        Set<String> friends2 = new HashSet<>();
        for(String f : friends){
            friends2.add(f);
        }
        this.friends=friends2;
    }


    
}


