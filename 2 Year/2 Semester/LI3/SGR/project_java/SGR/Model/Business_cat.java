package Model;

import java.io.Serializable;
import java.util.*;

public class Business_cat implements Serializable{
	private static final long serialVersionUID = -1967031705700725260L;

	private Map<String,Business> b_catalog;
	private int n_businesses;

	/**
	 * Constructor of Business_cat Class
	 * @param b_catalog - Map
	 * @param n_businesses - Int
	 */

	public Business_cat(Map<String, Business> b_catalog, int n_businesses) {
		this.b_catalog = b_catalog;
		this.n_businesses = n_businesses;
	}
/**
	 * Constructor of Business_cat Class
	 * @param b - Business_cat
	 */
	public Business_cat(Business_cat b) {
		this.b_catalog = b.get_b_catalog();
		this.n_businesses = b.get_n_businesses();
	}
	/**
	 * Empty constructor of Business_cat Class
	 */
	public Business_cat() {
		this.b_catalog = new TreeMap<>();
		this.n_businesses = 0;
	}
	/**
	 * Allows access to said variable
	 * @return b_catalog - Map
	 */
	public Map<String, Business> get_b_catalog() {
		return this.b_catalog;
	}
	/**
	 * Allows change of said variable
	 * @param b_catalog - Map
	 */
	public void set_b_catalog(Map<String, Business> b_catalog) {
		b_catalog.entrySet().forEach(u -> { this.b_catalog.put(u.getKey(), u.getValue().clone());});
	}
	/**
	 * Allows access to said variable
	 * @return n_businesses - Int
	 */
	public int get_n_businesses() {
		return this.n_businesses;
	}
/**
	 * Allows change of said variable
	 * @param n_businesses - Int
	 */
	public void set_n_businesses(int n_businesses) {
		this.n_businesses = n_businesses;
	}
/**
	 * Transforms the object to a String
	 * @return String
	 */
	public String toString() {
		StringBuilder s = new StringBuilder();
		for (String id : this.b_catalog.keySet()) {
			s.append("\nBusiness ID: ");
			s.append(id);
			s.append("\n");
			s.append(this.b_catalog.get(id).toString());
		}
        	s.append("\nTotal number of Users: ");
        	s.append(this.n_businesses);
        	return super.toString();
	}
	/**
	 * Clones the object Business_cat
	 * @return Business_cat
	 */
	public Business_cat clone(){
		return new Business_cat(this);
	}
	/**
	 * Method that compares one Object to another
	 * @return Boolean
	 */
	public boolean equals(Object obj) {
		if (obj == this) return true;
		if (obj == null || obj.getClass() != this.getClass()) return false;
		Business_cat b1 = (Business_cat) obj;
		return (b1.get_b_catalog().size() == this.b_catalog.size() &&
			b1.get_n_businesses() == this.n_businesses &&
			b1.get_b_catalog().equals(this.b_catalog));
	}
/**
	 * Method that Increments the n_businesses
 	 * @param id - String
     * @param bus - Business
	 */
	public void put(String id, Business bus){
		this.b_catalog.put(id, bus);
	}

	public void increment_n(){
		this.n_businesses++;
	}
/**
	 * Method that checks if the Catalog contains a ID
	 * @param id - String
	 * @return Boolean
	 */
	public boolean contains(String id){
		return this.b_catalog.containsKey(id);
    }
}
