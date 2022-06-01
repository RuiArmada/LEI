package Util.Exceptions;

@SuppressWarnings("serial")

public class Invalid_Review_Exception extends Exception {
	private String user_id;
	private String business_id;
	private String review_id;
	private boolean is_business;
	private boolean is_user;


	public Invalid_Review_Exception(String user_id, String business_id, String review_id, boolean is_business, boolean is_user) {
		super();
		this.user_id = user_id;
		this.business_id = business_id;
		this.review_id = review_id;
		this.is_business = is_business;
		this.is_user = is_user;
	}

	public String getUser_id() {
		return user_id;
	}

	public void setUser_id(String user_id) {
		this.user_id = user_id;
	}

	public String getBusiness_id() {
		return business_id;
	}

	public void setBusiness_id(String business_id) {
		this.business_id = business_id;
	}

	public String getReview_id() {
		return review_id;
	}

	public void setReview_id(String review_id) {
		this.review_id = review_id;
	}

	public boolean isIs_business() {
		return is_business;
	}

	public void setIs_business(boolean is_business) {
		this.is_business = is_business;
	}

	public boolean isIs_user() {
		return is_user;
	}

	public void setIs_user(boolean is_user) {
		this.is_user = is_user;
	}

	public String toString() {
		StringBuilder s = new StringBuilder();
		s.append("\n The Review with the ID:");
		s.append(this.review_id);
		s.append("\nCould not be loaded due to ");
		if(!is_business) {
			s.append("\nthe business:");
			s.append(this.business_id);
			s.append("\nbeing non-existent.");
		} 
		else{
			s.append("\nthe user:");
			s.append(this.user_id);
			s.append("\nbeing non-existent.");
		}
		return super.toString();
	}
}
