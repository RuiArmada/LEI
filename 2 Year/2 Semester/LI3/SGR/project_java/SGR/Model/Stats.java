package Model;

import java.util.HashMap;
import java.util.Map;

import Util.Triple;

public class Stats{
	private Triple<String,String,String> files;
	private int wrong_reviews;
	private int number_reviews;
	private int number_businesses;
	private int number_users;
	private int number_reviewed_businesses;
	private int number_non_reviewed_businesses;
	private int number_users_with_reviews;
	private int number_users_with_no_reviews;
	private int null_reviews;
	private Map<Integer,Integer> reviews_per_month;
	private Map<Integer,Float> average_per_month; 
	private double global_average;
	private Map<Integer,Integer> users_per_month;

/**
	 * Constructor for the Stats Class
	 * @param files - Triple
	 * @param wrong_reviews - Int
	 * @param number_reviews - Int
	 * @param number_businesses - Int
	 * @param number_users - Int
	 * @param number_reviewed_businesses - Int
	 * @param number_non_reviewed_businesses - Int
	 * @param number_users_with_reviews - Int
	 * @param number_users_with_no_reviews - Int
	 * @param null_reviews - Int
	 * @param reviews_per_month - Map
	 * @param average_per_month - Map
	 * @param global_average - Double
	 * @param users_per_month - Map
	 */
	public Stats(Triple<String, String, String> files, int wrong_reviews, int number_reviews, int number_businesses, int number_users, int number_reviewed_businesses, int number_non_reviewed_businesses, int number_users_with_reviews, int number_users_with_no_reviews, int null_reviews,Map<Integer,Integer> reviews_per_month,Map<Integer,Float> average_per_month, double global_average, Map<Integer,Integer> users_per_month) {
		this.files = files;
		this.wrong_reviews = wrong_reviews;
		this.number_reviews = number_reviews;
		this.number_businesses = number_businesses;
		this.number_users = number_users;
		this.number_reviewed_businesses = number_reviewed_businesses;
		this.number_non_reviewed_businesses = number_non_reviewed_businesses;
		this.number_users_with_reviews = number_users_with_reviews;
		this.number_users_with_no_reviews = number_users_with_no_reviews;
		this.null_reviews = null_reviews;
		setUsers_per_month(users_per_month);
		this.global_average = global_average;
		setReviews_per_month(reviews_per_month);
		setAverage_per_month(average_per_month);
	}
/**
	 * Constructor for the Stats Class
	 * @param s - Stats
	 */
	public Stats(Stats s) {
		this.files = s.getFiles();
		this.wrong_reviews = s.getWrong_reviews();
		this.number_reviews = s.getNumber_reviews();
		this.number_businesses = s.getNumber_businesses();
		this.number_users = s.getNumber_users();
		this.number_reviewed_businesses = s.getNumber_reviewed_businesses();
		this.number_non_reviewed_businesses = s.getNumber_non_reviewed_businesses();
		this.number_users_with_reviews = s.getNumber_users_with_reviews();
		this.number_users_with_no_reviews = s.getNumber_users_with_no_reviews();
		this.null_reviews = s.getNull_reviews();
		this.reviews_per_month = s.getReviews_per_month();
		this.global_average = s.getGlobal_average();
		this.average_per_month = s.getAverage_per_month();
		this.users_per_month = this.getUsers_per_month();
	}
/**
	 * Empty Constructor for the Stats Class
	 */
	public Stats() {
			this.files = new Triple<>(" "," "," ");
			this.wrong_reviews = 0;
			this.number_reviews = 0;
			this.number_businesses = 0;
			this.number_users = 0;
			this.number_reviewed_businesses = 0;
			this.number_non_reviewed_businesses = 0;
			this.number_users_with_reviews = 0;
			this.number_users_with_no_reviews = 0;
			this.null_reviews = 0;
			this.reviews_per_month = new HashMap<>();
			this.average_per_month = new HashMap<>();
			this.global_average = 0;
			this.users_per_month = new HashMap<>();
		}
/**
		 * Allows access to said variable
		 * @return file - Triple
		 */
		public Triple<String, String, String> getFiles() {
			return files;
		}
/**
		 * Allows the change of said variable
		 * @param files - Triple
		 */
		public void setFiles(Triple<String, String, String> files) {
			this.files = files;
		}
/**
		 * Allows access to said variable
		 * @return wrong_reviews - Int
		 */
		public int getWrong_reviews() {
			return wrong_reviews;
		}
	/**
		 * Allows the change of said variable
		 * @param wrong_reviews - Int
		 */
		public void setWrong_reviews(int wrong_reviews) {
			this.wrong_reviews = wrong_reviews;
		}
/**
		 * Allows access to said variable
		 * @return number_reviews - Int
		 */
		public int getNumber_reviews() {
			return number_reviews;
		}
	/**
		 * Allows the change of said variable
		 * @param number_reviews - Int
		 */
		public void setNumber_reviews(int number_reviews) {
			this.number_reviews = number_reviews;
		}
/**
		 * Allows access to said variable
		 * @return number_businesses - Int
		 */
		public int getNumber_businesses() {
			return number_businesses;
		}
/**
		 * Allows the change of said variable
		 * @param number_businesses - Int
		 */
		public void setNumber_businesses(int number_businesses) {
			this.number_businesses = number_businesses;
		}
/**
		 * Allows access to said variable
		 * @return number_users - Int
		 */
		public int getNumber_users() {
			return number_users;
		}
/**
		 * Allows the change of said variable
		 * @param number_users - Int
		 */
		public void setNumber_users(int number_users) {
			this.number_users = number_users;
		}
/**
		 * Allows access to said variable
		 * @return number_reviewed_businesses - Int
		 */
		public int getNumber_reviewed_businesses() {
			return number_reviewed_businesses;
		}
/**
		 * Allows the change of said variable
		 * @param number_reviewed_businesses - Int
		 */
		public void setNumber_reviewed_businesses(int number_reviewed_businesses) {
			this.number_reviewed_businesses = number_reviewed_businesses;
		}
/**
		 * Allows access to said variable
		 * @return number_non_reviewed_businesses - Int
		 */
		public int getNumber_non_reviewed_businesses() {
			return number_non_reviewed_businesses;
		}
/**
		 * Allows the change of said variable
		 * @param number_non_reviewed_businesses - Int
		 */
		public void setNumber_non_reviewed_businesses(int number_non_reviewed_businesses) {
			this.number_non_reviewed_businesses = number_non_reviewed_businesses;
		}
/**
		 * Allows access to said variable
		 * @return number_users_with_reviews - Int
		 */
		public int getNumber_users_with_reviews() {
			return number_users_with_reviews;
		}
/**
		 * Allows the change of said variable
		 * @param number_users_with_reviews - Int
		 */
		public void setNumber_users_with_reviews(int number_users_with_reviews) {
			this.number_users_with_reviews = number_users_with_reviews;
		}
/**
		 * Allows access to said variable
		 * @return number_users_with_reviews - Int
		 */
		public int getNumber_users_with_no_reviews() {
			return number_users_with_no_reviews;
		}
/**
		 * Allows the change of said variable
		 * @param number_users_with_no_reviews - Int
		 */
		public void setNumber_users_with_no_reviews(int number_users_with_no_reviews) {
			this.number_users_with_no_reviews = number_users_with_no_reviews;
		}
/**
		 * Allows access to said variable
		 * @return null_reviews - Int
		 */
		public int getNull_reviews() {
			return null_reviews;
		}
/**
		 * Allows the change of said variable
		 * @param null_reviews - Int
		 */
		public void setNull_reviews(int null_reviews) {
			this.null_reviews = null_reviews;
		}
/**
	 * Allows access to said variable
	 * @return reviews_per_month - Map
	 */
	public Map<Integer, Integer> getReviews_per_month() {
		return reviews_per_month;
	}
/**
	 * Allows the change of said variable
	 * @param reviews_per_month - Map
	 */
	public void setReviews_per_month(Map<Integer, Integer> reviews_per_month) {
		reviews_per_month.entrySet().forEach(u->{this.reviews_per_month.put(u.getKey(),u.getValue());});
	}
/**
	 * Allows access to said variable
	 * @return average_per_month - Map
	 */
	public Map<Integer, Float> getAverage_per_month() {
		return this.average_per_month;
	}
/**
	 * Allows the change of said variable
	 * @param average_per_month - Map
	 */
	public void setAverage_per_month(Map<Integer, Float> average_per_month) {
		average_per_month.entrySet().forEach(u->{this.average_per_month.put(u.getKey(), u.getValue());});
	}
/**
	 * Allows access to said variable
	 * @return global_average - Double
	 */
	public double getGlobal_average() {
		return global_average;
	}
/**
	 * Allows the change of said variable
	 * @param global_average - Double
	 */
	public void setGlobal_average(double global_average) {
		this.global_average = global_average;
	}
	/**
	 * Allows access to said variable
	 * @return users_per_month - Map
	 */
	public Map<Integer, Integer> getUsers_per_month() {
		return users_per_month;
	}
/**
	 * Allows the change of said variable
	 * @param users_per_month - Map
	 */
	public void setUsers_per_month(Map<Integer, Integer> users_per_month) {
		users_per_month.entrySet().forEach(u->{this.users_per_month.put(u.getKey(),u.getValue());});
	}
}

