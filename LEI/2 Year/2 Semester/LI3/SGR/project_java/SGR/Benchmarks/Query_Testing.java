package Benchmarks;

import java.lang.Runtime;

import Util.Crono;
import Controler.Parser;
import Model.Model_SGR;
import Model.Table;

public class Query_Testing {

	private static final long MEGABYTE = 1024L * 1024L;
	public static long bytesToMegabytes(long bytes) {
		return bytes / MEGABYTE;
	}


    public static void Testing() {

        Model_SGR m = new Model_SGR();
        Parser p = new Parser();

        /*
            Parsing - With Friends
        */
        try {
           
            Crono.start();
	    long init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
            p.open_files();
            p.parse_full(m);
            Crono.stop();
            System.out.print("Parsing Time -> " + Crono.get_time_to_stringln());
	    long out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	    System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb\n");

        } catch(IndexOutOfBoundsException e) {
            System.out.println("Wrong file path");
        }

        /*
            Query 1
        */
	long init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query1 = m.query1();
        Crono.stop();
        System.out.println("Query 1 -> " + Crono.get_time_to_string());
	long out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        System.out.println("Query 1: " + query1 + "\n");


        /*
            Query 2 - Valid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query2_1 = m.query2(2000, 07);
        Crono.stop();
        System.out.println("Query 2 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query2_1.isEmpty()) {
            System.out.println("Query 2: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid." + "\n");
        } else {
            System.out.println("Query 2: " + query2_1);
        }

        /*
            Query 2 - Invalid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query2_2 = m.query2(1998, 07);
        Crono.stop();
        System.out.println("Query 2 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query2_2.isEmpty()) {
            System.out.println("Query 2: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid."+ "\n");
        } else {
            System.out.println("Query 2: " + query2_2 + "\n");
        }

        /* 
            Query 3 - Valid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        String u1 = "37Hc8hr3cw0iHLoPzLK6Ow";
        Crono.start();
        Table query3_1 = m.query3(u1);
        Crono.stop();
        System.out.println("Query 3 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query3_1.isEmpty()) {
            System.out.println("Query 3: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 3: " + query3_1);
        }

        /* 
            Query 3 - Invalid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        String u2 = "1u";
        Crono.start();
        Table query3_2 = m.query3(u2);
        Crono.stop();
        System.out.println("Query 3 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query3_2.isEmpty()) {
            System.out.println("Query 3: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 3: " + query3_2 + "\n");
        }

        /*
            Query 4 - Valid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        String b1 = "6iYb2HFDywm3zjuRg0shjw";
        Crono.start();
        Table query4_1 = m.query4(b1);
        Crono.stop();
        System.out.println("Query 4 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query4_1.isEmpty()) {
            System.out.println("Query 4: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 4: " + query4_1);
        }

        /*
            Query 4 - Invalid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        String b2 = "1b";
        Crono.start();
        Table query4_2 = m.query4(b2);
        Crono.stop();
        System.out.println("Query 4 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query4_2.isEmpty()) {
            System.out.println("Query 4: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 4: " + query4_2 + "\n");
        }

        /*
            Query 5 - Valid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query5_1 = m.query5(u1);
        Crono.stop();
        System.out.println("Query 5 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query5_1.isEmpty()) {
            System.out.println("Query 5: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 5: " + query5_1);
        }

        /*
            Query 5 - Invalid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query5_2 = m.query5(u2);
        Crono.stop();
        System.out.println("Query 5 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query5_2.isEmpty()) {
            System.out.println("Query 5: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid." + "\n");
        } else {
            System.out.println("Query 5: " + query5_2 + "\n");
        }

        /*
            Query 6 - Valid Test 
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query6_1 = m.query6(5);
        Crono.stop();
        System.out.println("Query 6 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query6_1.isEmpty()) {
            System.out.println("Query 6: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 6: " + query6_1);
        }

        /*
            Query 6 - Invalid Test 
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query6_2 = m.query6(-5);
        Crono.stop();
        System.out.println("Query 6 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query6_2.isEmpty()) {
            System.out.println("Query 6: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 6: " + query6_2 + "\n");
        }

        /*
            Query 7
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query7 = m.query7();
        Crono.stop();
        System.out.println("Query 7 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query7.isEmpty()) {
            System.out.println("Query 7: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 7: " + query7 + "\n");
        }

        /*
            Query 8 - Valid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query8_1 = m.query8(5);
        Crono.stop();
        System.out.println("Query 8 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query8_1.isEmpty()) {
            System.out.println("Query 8: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 8: " + query8_1);
        }

        /*
            Query 8 - Invalid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query8_2 = m.query8(-5);
        Crono.stop();
        System.out.println("Query 8 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query8_2.isEmpty()) {
            System.out.println("Query 8: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 8: " + query8_2 + "\n");
        }

        /*
            Query 9 - Valid Test
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query9_1 = m.query9(b1, 5);
        Crono.stop();
        System.out.println("Query 9 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query9_1.isEmpty()) {
            System.out.println("Query 9: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 9: " + query9_1);
        }

        /*
            Query 9 - Invalid Test (By BusinessID)
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query9_2 = m.query9(b2, 5);
        Crono.stop();
        System.out.println("Query 9 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query9_2.isEmpty()) {
            System.out.println("Query 9: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 9: " + query9_2);
        }
        
        /*
            Query 9 - Invalid Test (By Integer)
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query9_3 = m.query9(b1, -5);
        Crono.stop();
        System.out.println("Query 9 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query9_3.isEmpty()) {
            System.out.println("Query 9: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 9: " + query9_3);
        }

        /*
            Query 9 - Invalid Test (By Both Arguments)
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query9_4 = m.query9(b2, -5);
        Crono.stop();
        System.out.println("Query 9 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query9_4.isEmpty()) {
            System.out.println("Query 9: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 9: " + query9_4 + "\n");
        }

        /*
            Query 10
        */
	init = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
        Crono.start();
        Table query10 = m.query10();
        Crono.stop();
        System.out.println("Query 10 -> " + Crono.get_time_to_string());
	out = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
	System.out.println("Used Memory: " + bytesToMegabytes(out-init) + "Mb");
        if(query10.isEmpty()) {
            System.out.println("Query 10: Recieved Empty Table");
            System.out.println("Therefore, the test was invalid.");
        } else {
            System.out.println("Query 10: " + query10 + "\n");
        }

    }

}
