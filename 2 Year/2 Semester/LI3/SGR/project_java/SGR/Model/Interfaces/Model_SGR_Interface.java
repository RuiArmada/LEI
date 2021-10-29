package Model.Interfaces;

import java.util.Date;
import java.util.Set;

import Model.Business_cat;
import Model.Review_cat;
import Model.Table;
import Model.User_cat;

public interface Model_SGR_Interface {
    
    public Business_cat getBusinesses();
    public Review_cat getReviews();
    public User_cat getUsers();
    public void setUsers(User_cat users);
    public void setBusinesses(Business_cat businesses);
    public void addUser(String id, String name);
    public void addUser(String id, String name, Set<String> friends);
    public void add_business(String id, String name, String city, String state, Set<String> categories);
    public void add_review(String r_id, String b_id, String u_id, float stars, int useful, int funny, int cool, Date date, String text);
    public Table query1();
    public Table query2(int year, int month);
    public Table query3(String user_id);
    public Table query4(String business_id);
    public Table query5(String user_id);
    public Table query6(int x);
    public Table query7();
    public Table query8(int x);
    public Table query9(String business_id, int x);
    public Table query10();

}
