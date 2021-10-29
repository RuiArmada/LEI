package Benchmarks;

import Util.Exceptions.NonExisting_Business_Exception;
import Util.Exceptions.NonExisting_Review_Exception;
import Util.Exceptions.NonExisting_User_Exception;
import Benchmarks.Unitary.Business_Test;
import Benchmarks.Unitary.Review_Test;
import Benchmarks.Unitary.User_Test;
import Model.Business_cat;
import Model.Review_cat;
import Model.User_cat;
import View.View_SGR;

public class Benchmarks {
    
    /**
     * Method that tests all functions of the application
     */
    public static void benchmarks() {
        View_SGR v = new View_SGR();
        User_cat u = new User_cat();
        Business_cat b = new Business_cat();
        Review_cat r = new Review_cat();

        v.benchmark_begin();

        // Query Testing
        
        Query_Testing.Testing();

        // Unitary Testing

        /* 
            USER
        */
        
        try {
            if(User_Test.containsUser(u,"37Hc8hr3cw0iHLoPzLK6Ow")) {
                System.out.println("User exists.");
            }
        } catch (NonExisting_User_Exception e) {
            System.out.println("User does not exist.");
        }
        
        try {
            if(User_Test.containsUser(u,"u1")) {
                System.out.println("User exists.");
            }
        } catch (NonExisting_User_Exception e) {
            System.out.println("User does not exist.");
        }
            
        if(User_Test.existingName(u,"Jane")) {
                System.out.println("User exists.");
        } else {
            System.out.println("User does not exist.");
        }

        if(User_Test.existingName(u,"Gertrudes")) {
            System.out.println("User exists.");
        } else {
            System.out.println("User does not exist.");
        }

        /* 
            BUSINESS
        */

        try {
            if(Business_Test.containsBus(b, "6iYb2HFDywm3zjuRg0shjw")) {
                System.out.println("Business exists.");
            }
        } catch (NonExisting_Business_Exception e) {
            System.out.println("Business does not exist.");
        }

        try {
            if(Business_Test.containsBus(b, "b1")) {
                System.out.println("Business exists.");
            }
        } catch (NonExisting_Business_Exception e) {
            System.out.println("Business does not exist.");
        }

        if(Business_Test.existingName(b, "Oskar Blues Taproom")) {
            System.out.println("Business exists.");
        } else {
            System.out.println("Business does not exist.");
        }

        if(Business_Test.existingName(b, "Guitar N Co Lda")) {
            System.out.println("Business exists.");
        } else {
            System.out.println("Business does not exist.");
        }

        /*
            REVIEWS
        */
        try {
            if(Review_Test.containsRev(r, "lWC-xP3rd6obsecCYsGZRg")) {
                System.out.println("Review exists.");
            }
        } catch (NonExisting_Review_Exception e) {
            System.out.println("Review does not exist.");
        }

        try {
            if(Review_Test.containsRev(r, "r1")) {
                System.out.println("Review exists.");
            }
        } catch (NonExisting_Review_Exception e) {
            System.out.println("Review does not exist.");
        }

        try {
            if(Review_Test.containsRevUserID(r, "ak0TdVmGKo4pwqdJSTLwWw")) {
                System.out.println("User exists in Reviews.");
            }
        } catch (NonExisting_User_Exception e) {
            System.out.println("User does not exist in Reviews.");
        }

        try {
            if(Review_Test.containsRevUserID(r, "u1")) {
                System.out.println("User exists in Reviews.");
            }
        } catch (NonExisting_User_Exception e) {
            System.out.println("User does not exist in Reviews.");
        }

        try {
            if(Review_Test.containsRevBusID(r, "buF9druCkbuXLX526sGELQ")) {
                System.out.println("Business exists in Reviews.");
            }
        } catch (NonExisting_Business_Exception e) {
            System.out.println("Business does not exist in Reviews.");
        }

        try {
            if(Review_Test.containsRevBusID(r, "b1")) {
                System.out.println("Business exists in Reviews.");
            }
        } catch (NonExisting_Business_Exception e) {
            System.out.println("Business does not exist in Reviews.");
        }

        v.benchmark_end();
    }
}
