package Model.Interfaces;

import java.util.Set;

import Model.User;

public interface User_Interface {
    
    public String toString();
    public User clone();
    public boolean equals(Object obj);
    public String getUser_name();
    public Set<String> getFriends();
    public int getN_reviews();
    public void setN_reviews(int n_reviews);
    public void setUser_name(String user_name);
    public void setFriends(Set<String> friends);

}
