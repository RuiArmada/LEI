package Model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;


//reviews ordenadas por user id
public class Review_cat implements Serializable{
    private static final long serialVersionUID = -6899927918606892807L;

    private Map<String,Review> r_catalog;
    private int n_reviews;

/**
	 * Empty Constructor for the Review_cat Class
	 */
    public Review_cat(){
        this.r_catalog = new TreeMap<>();
        this.n_reviews = 0;
    }
/**
     * Constructor for the Review_cat Class
     * @param uRev - Review_cat
     */
    public Review_cat(Review_cat uRev){
        setR_catalog(uRev.getR_catalog());
        this.n_reviews = uRev.getN_reviews();
    }
/**
     * Constructor for the Review_cat Class
     * @param r_catalog - Map
     * @param n_reviews - Int
     */
    public Review_cat(Map<String,Review> r_catalog, int n_reviews){
        setR_catalog(r_catalog);
        this.n_reviews = n_reviews;
    }
 /**
	 * Transforms the object to a String
	 * @return String
	 */
    public String toString() {
        StringBuilder s = new StringBuilder();
        for (String id : this.r_catalog.keySet()) {
            s.append("\nUser ID: ");
            s.append(id);
            s.append("\n");
            s.append(this.r_catalog.get(id).toString());
        }
        s.append("\nTotal number of Users: ");
        s.append(this.n_reviews);
        return super.toString();
    }
/**
	 * Clones the object Review_cat
	 * @return Review_cat
	 */
    public Review_cat clone() {
        return new Review_cat(this);
    }
/**
	 * Method that compares one Object to another
	 * @return Boolean
	 */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Review_cat r1 = (Review_cat) obj;
        return r1.getN_reviews() == this.n_reviews &&
               r1.getR_catalog().equals(this.r_catalog);
    }
   
    //----------------------------- gets e sets ------------------------------------------
 /**
     * Allows access to said variable
     * @return n_reviews - Int
     */
    public int getN_reviews() {
        return this.n_reviews;
    }
    /**
     * Allows access to said variable
     * @return r_catalog - Map
     */
    public Map<String, Review> getR_catalog() {
        return this.r_catalog;
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
     * @param r_catalog - Map
     */
    public void setR_catalog(Map<String, Review> r_catalog) {
        r_catalog.entrySet()
                    .forEach(u -> { this.r_catalog.put(u.getKey(), u.getValue().clone());});
    }

    //----------------------------------------------- methods ------------------------------------------------------
 /**
     * Method that gets the Reviews made by a certain User
     * @param userID - String
     * @return listB - List
     */
    public List<String> getReviewsByUser(String userID){
        List<String> listB = new ArrayList<>();

        for (Entry<String,Review> entry : this.r_catalog.entrySet()) {
            if (entry.getValue().get_user_id().equals(userID)){
                listB.add(entry.getValue().getBusiness_id());
            }
        }

        return listB;
    }
 /**
     * Method that inserts a Review to the Catalog
     * @param id - String
     * @param rev - Review
     */
    public void put(String id, Review rev){
	    this.r_catalog.put(id, rev);
    }
     /**
     * Method that Increments the n_reviews
     */
    public void increment(){
	    this.n_reviews++;
    }
/**
     * Method that checks if the Catalog contains a ID
     * @param id - String
     * @return Boolean
     */
    public boolean contains(String id){
        return this.r_catalog.containsKey(id);
    }
}

