package Model.Interfaces;

import java.util.Set;

import Model.Business;

public interface Business_Interface {
    
    public String toString();
    public Business clone();
    public boolean equals(Object obj);
    public String get_business_name();
    public void set_business_name(String business_name);
    public String get_business_city();
    public void set_business_city(String business_city);
    public String get_business_state();
    public void set_business_state(String business_state);
    public Set<String> get_business_cat();
    public void set_business_cat(Set<String> business_cat);
    public int get_n_reviews();
    public void set_n_reviews(int n_reviews);
    public float get_m_stars();
    public void set_m_stars(float m_stars);

}
