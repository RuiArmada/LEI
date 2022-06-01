package Model;

import java.io.Serializable;
import java.util.Date;

public class Review implements Serializable{
    private static final long serialVersionUID = 333414744556871475L;

    private String user_id;
    private String business_id;
    private float stars;
    private int useful;
    private int funny;
    private int cool;
    private Date date;
    private String text;
/**
	 * Empty Constructor for the Review Class
	 */
    public Review(){
        this.user_id = "undefined";
        this.business_id = "undefined";
        this.stars = 0;
        this.useful = 0;
        this.funny = 0;
        this.cool = 0;
        this.date = new Date(); 
        this.text = "undefined";
        
    }
    /**
     * Constructor for Review Class
     * @param r - Review
     */
    public Review(Review r){
        this.user_id = r.get_user_id();
        this.business_id = r.getBusiness_id();
        this.stars = r.getStars();
        this.useful = r.getUseful();
        this.funny = r.getFunny();
        this.cool = r.getCool();
        this.date = r.getDate();
        this.text = r.getText();
    }
 /**
     * Constructor for Review Class
     * @param user_id - String
     * @param business_id - String
     * @param stars - Float
     * @param useful - Int
     * @param funny - Int
     * @param cool - Int
     * @param date - Date
     * @param text - String
     */
    public Review(String user_id, String business_id, float stars, int useful,int funny,int cool,Date date,String text) {
        this.user_id = user_id;
        this.business_id = business_id;
        this.stars = stars;
        this.useful = useful;
        this.funny = funny;
        this.cool = cool;
        this.date = date;
        this.text = text;
    }
/**
     * Constructor for Review Class
     * @param user_id - String
     * @param business_id - String
     * @param stars - Float
     * @param date - Date
     * @param text - String
     */
    public Review(String user_id, String business_id, float stars,Date date,String text) {
        this.user_id = user_id;
        this.business_id = business_id;
        this.stars = stars;
        this.date = date;
        this.text = text;
    }
 /**
	 * Clones the object Review
	 * @return Review
	 */
    public Review clone(){
		return new Review(this);
	}
 /**
	 * Transforms the object to a String
	 * @return String
	 */
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("\nUser ID: ");
        s.append(this.user_id);
        s.append("\nBusiness ID: ");
        s.append(this.business_id);
        s.append("\nUser ID: ");
        s.append(this.stars);
        s.append("\nUsefulness: ");
        s.append(this.useful);
        s.append("\nFunny: ");
        s.append(this.funny);
        s.append("\nCool: ");
        s.append(this.cool);
        s.append("\nDate: ");
        s.append(this.date);
        s.append("\nText: ");
        s.append(this.text);
        	return super.toString();      
    }
/**
	 * Method that compares one Object to another
	 * @return Boolean
	 */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Review r1 = (Review) obj;
        return r1.get_user_id().equals(this.user_id) &&
               r1.getBusiness_id().equals(this.business_id) &&
               r1.getStars() == this.stars &&
               r1.getUseful() == this.useful &&
               r1.getFunny() == this.funny &&
               r1.getCool() == this.cool &&
               r1.getDate() == this.date &&
               r1.getText() == this.text;
    }

    //----------------------------------------- gets & sets  -----------------------------------------
/**
     * Allows access to said variable
     * @return business_id - String
     */
    public String getBusiness_id() {
        return this.business_id;
    }
    /**
     * Allows access to said variable
     * @return user_id - String
     */
    public String get_user_id() {
        return this.user_id;
    }
   /**
     * Allows access to said variable
     * @return cool - Int
     */
    public int getCool() {
        return this.cool;
    }
    /**
     * Allows access to said variable
     * @return date - Date
     */
    public Date getDate() {
        return this.date;
    }
    /**
     * Allows access to said variable
     * @return funny - Int
     */
    public int getFunny() {
        return this.funny;
    }
    /**
     * Allows access to said variable
     * @return stars - Float
     */
    public float getStars() {
        return this.stars;
    }
    /**
     * Allows access to said variable
     * @return text - String
     */
    public String getText() {
        return this.text;
    }
    /**
     * Allows access to said variable
     * @return usefull - Int
     */
    public int getUseful() {
        return this.useful;
    }

/**
     * Allows the change of said variable
     * @param business_id - String
     */
    public void setBusiness_id(String business_id) {
        this.business_id = business_id;
    }
    /**
     * Allows the change of said variable
     * @param cool - Int
     */
    public void setCool(int cool) {
        this.cool = cool;
    }
    /**
     * Allows the change of said variable
     * @param date - Date
     */
    public void setDate(Date date) {
        this.date = date;
    }
    /**
     * Allows the change of said variable
     * @param funny - Int
     */
    public void setFunny(int funny) {
        this.funny = funny;
    }
    /**
     * Allows the change of said variable
     * @param user_id - String
     */
    public void set_user_id(String user_id) {
        this.user_id = user_id;
    }
     /**
     * Allows the change of said variable
     * @param stars - Float
     */
    public void setStars(float stars) {
        this.stars = stars;
    }
    /**
     * Allows the change of said variable
     * @param text - String
     */
    public void setText(String text) {
        this.text = text;
    }
    /**
     * Allows the change of said variable
     * @param useful - Int
     */
    public void setUseful(int useful) {
        this.useful = useful;
    }
/**
     * Allows access to said variable
     * @return year - Int
     */
    public int getYearR(){
        return this.date.getYear()+1903;
    }
/**
     * Allows access to said variable
     * @return month - Int
     */
    public int getMonthR(){
        return this.date.getMonth()+1;
    }

}
