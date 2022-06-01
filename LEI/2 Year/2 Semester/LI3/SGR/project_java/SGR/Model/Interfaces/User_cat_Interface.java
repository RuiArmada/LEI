package Model.Interfaces;

import java.util.Map;

import Model.User;
import Model.User_cat;

public interface User_cat_Interface {
    
    public String toString();
    public User_cat clone();
    public boolean equals(Object obj);
    public int getN_users();
    public Map<String, User> getU_catalog();
    public void setN_users(int n_users);
    public void setU_catalog(Map<String, User> u_catalog);

}
