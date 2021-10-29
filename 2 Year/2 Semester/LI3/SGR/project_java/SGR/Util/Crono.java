package Util;

import static java.lang.System.nanoTime;

public class Crono{

    private static long inicio = 0L;
    private static long fim = 0L;
    /**
     * Empty constructor for the Crono class
     */
    public Crono(){
	    inicio = 0L;
	    fim = 0L;
    }
    /**
     * Method that starts the timer
     */
    public static void start() {
        fim = 0L; inicio = nanoTime();
    }
    /**
     * Method that ends the timer
     * @return time - Double
     */
    public static double stop() {
        fim = nanoTime();
        long elapsedTime = fim - inicio;
        return elapsedTime / 1.0E09;
    }
    /**
     * Method that get's the time and transforms it into a string
     * @return string - String
     */
    public static String get_time() {
        return "" + stop();
    }
    /**
     * Method that beautifies the get_time() method
     * @return string - String
     */
    public static String get_time_to_string() {
        return "Elapsed Time: " +get_time() + " s";
    }
    /**
     * Method that beautifies the get_time() method - adds a \n in the end
     * @return string - String
     */
    public static String get_time_to_stringln() {
        return "Elapsed Time: " +get_time() + " s\n";
    }
}
