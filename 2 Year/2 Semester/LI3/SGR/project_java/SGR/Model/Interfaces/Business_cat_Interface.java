package Model.Interfaces;

import java.util.Map;

import Model.Business;
import Model.Business_cat;

public interface Business_cat_Interface {

    public String toString();
    public Business_cat clone();
    public boolean equals(Object obj);
    public Map<String, Business> get_b_catalog();
    public void set_b_catalog(Map<String, Business> b_catalog);
    public int get_n_businesses();
    public void set_n_businesses(int n_businesses);
    
}
