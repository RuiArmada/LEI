package Benchmarks.Unitary;

import java.util.Map.Entry;

import Util.Exceptions.NonExisting_Business_Exception;
import Model.Business;
import Model.Business_cat;

public class Business_Test {
    
    public static boolean containsBus(Business_cat b, String id) throws NonExisting_Business_Exception {
        return b.get_b_catalog().keySet().contains(id);
    }

    public static boolean existingName(Business_cat b, String name) {
        boolean rez = false;
        for(Entry<String, Business> entry : b.get_b_catalog().entrySet()) {
            if(entry.getValue().get_business_name() == name) {
                rez = true;
            }
        }
        return rez;
    }

}
