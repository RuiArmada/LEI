package Benchmarks.Unitary;

import java.util.Map.Entry;

import Util.Exceptions.NonExisting_Business_Exception;
import Util.Exceptions.NonExisting_Review_Exception;
import Util.Exceptions.NonExisting_User_Exception;
import Model.Review;
import Model.Review_cat;

public class Review_Test {

    public static boolean containsRev(Review_cat r, String id) throws NonExisting_Review_Exception {
        return r.getR_catalog().keySet().contains(id);
    }

    public static boolean containsRevUserID(Review_cat r, String id) throws NonExisting_User_Exception {
        boolean rez = false;
        for(Entry<String, Review> entry : r.getR_catalog().entrySet()) {
            if(entry.getValue().get_user_id() == id) {
                rez = true;
            }
        }
        return rez;
    }

    public static boolean containsRevBusID(Review_cat r, String id) throws NonExisting_Business_Exception {
        boolean rez = false;
        for(Entry<String, Review> entry : r.getR_catalog().entrySet()) {
            if(entry.getValue().getBusiness_id() == id) {
                rez = true;
            }
        }
        return rez;
    }
    
}
