package Benchmarks.Unitary;

import java.util.Map.Entry;

import Util.Exceptions.NonExisting_User_Exception;
import Model.User;
import Model.User_cat;

public class User_Test {
    
     public static boolean containsUser(User_cat u, String id) throws NonExisting_User_Exception { 
        return u.getU_catalog().keySet().contains(id);  
     }

     public static boolean existingName(User_cat u, String name) {
         boolean rez = false;
         for(Entry<String, User> entry : u.getU_catalog().entrySet()) {
             if(entry.getValue().getUser_name() == name) {
                rez = true;
             }
         }
         return rez;
     }

}
