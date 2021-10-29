package Model;
import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

public class Business implements Serializable{
	private static final long serialVersionUID = 4024373428370124187L;

	private String business_name;
	private String business_city;
	private String business_state;
	private Set<String> business_cat;
	private int n_reviews;
	private float m_stars;
/**
	 * Constructor for the Business Class
	 * @param business_name - String
	 * @param business_city - String
	 * @param business_state - String
	 * @param business_cat - Set
	 * @param n_reviews - Int
	 * @param m_stars - Float
	 */
	public Business(String business_name, String business_city, String business_state, Set<String> business_cat, int n_reviews, float m_stars) {
		this.business_name = business_name;
		this.business_city = business_city;
		this.business_state = business_state;
		set_business_cat(business_cat);
		this.n_reviews = n_reviews;
		this.m_stars = m_stars;
	}
/**
	 * Constructor for the Business Class
	 * @param b - Business
	 */
	public Business(Business b) {
		this.business_name = b.get_business_name();
		this.business_city = b.get_business_city();
		this.business_state = b.get_business_state();
		set_business_cat(b.get_business_cat());
		this.n_reviews = b.get_n_reviews();
		this.m_stars = b.get_m_stars();
	}
/**
	 * Empty Constructor for the Business Class
	 */
	public Business() {
		this.business_name = "";
		this.business_city = "";
		this.business_state = "";
		this.business_cat = new HashSet<>();
		this.n_reviews = 0;
		this.m_stars = 0;
	}
/**
	 * Allows access to said variable
	 * @return business_name - String
	 */
	public String get_business_name() {
		return business_name;
	}
	/**
	 * Allows the change of said variable
	 * @param business_name - String
	 */
	public void set_business_name(String business_name) {
		this.business_name = business_name;
	}
/**
	 * Allows access to said variable
	 * @return business_city - String
	 */
	public String get_business_city() {
		return business_city;
	}
/**
	 * Allows the change of said variable
	 * @param business_city - String
	 */
	public void set_business_city(String business_city) {
		this.business_city = business_city;
	}
/**
	 * Allows access to said variable
	 * @return business_state - String
	 */
	public String get_business_state() {
		return business_state;
	}
/**
	 * Allows the change of said variable
	 * @param business_state - String
	 */
	public void set_business_state(String business_state) {
		this.business_state = business_state;
	}
/**
	 * Allows access to said variable
	 * @return business_cat - Set
	 */
	public Set<String> get_business_cat() {
		return business_cat;
	}
/**
	 * Allows the change of said variable
	 * @param business_cat - Set
	 */
	public void set_business_cat(Set<String> business_cat) {
		Set<String> cat = new HashSet<>();
		for(String c : business_cat){
			cat.add(c);
		}
		this.business_cat = cat;
	}
/**
	 * Allows access to said variable
	 * @return n_reviews - Int
	 */
	public int get_n_reviews() {
		return n_reviews;
	}
/**
	 * Allows the change of said variable
	 * @param n_reviews - Int
	 */
	public void set_n_reviews(int n_reviews) {
		this.n_reviews = n_reviews;
	}
	/**
	 * 
	 * @return m_stars - Float
	 */
	public float get_m_stars() {
		return m_stars;
	}
/**
	 * Allows the change of said variable
	 * @param m_stars - Float
	 */
	public void set_m_stars(float m_stars) {
		this.m_stars = m_stars;
	}
/**
	 * Method that increments the number of reviews
	 */
	public void plusRev(){
		this.n_reviews++;
	}
/**
	 * Transforms the object to a String
	 * @return String
	 */

	public String toString() {
        int n = 0;
        StringBuilder s = new StringBuilder();
        s.append("Business Name: ");
        s.append(this.business_name);
        s.append("\nId: ");
        s.append(this.business_state);
        s.append("\nCity: ");
        s.append(this.business_city);
        s.append("\nNumber of Reviews: ");
        s.append(this.n_reviews);
        s.append("\nAverage Stars: ");
        s.append(this.m_stars);
        if(this.business_cat.size()!=0){
            s.append("\nCategories: ");
            for (String cat : this.business_cat) {
                s.append(cat);
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
	 * Clones the object Business
	 * @return Business
	 */
    public Business clone(){
	    return new Business(this);
    }
/**
	 * Method that compares one Object to another
	 * @return Boolean
	 */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Business b1 = (Business) obj;
        return b1.get_business_name().equals(this.business_name) &&
               b1.get_business_city().equals(this.business_city) &&
               b1.get_business_state().equals(this.business_state) &&
               b1.get_n_reviews() == this.n_reviews &&
               b1.get_m_stars() == this.m_stars &&
               b1.get_business_cat().equals(this.business_cat);
    }

}
