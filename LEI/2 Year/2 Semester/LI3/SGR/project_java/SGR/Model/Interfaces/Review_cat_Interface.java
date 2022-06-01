package Model.Interfaces;

import java.util.Map;
import java.util.Set;

import Model.Review;
import Model.Review_cat;

public interface Review_cat_Interface {
    
    public String toString();
    public Review_cat clone();
    public boolean equals(Object obj);
    public int getN_reviews();
    public Map<String, Review> getR_catalog();
    public void setN_reviews();
    public void setR_catalog(Map<String, Review> r_catalog);
    public Set<String> getReviewsByUser(String userID);

}
