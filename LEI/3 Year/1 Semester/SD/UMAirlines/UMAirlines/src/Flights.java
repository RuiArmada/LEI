package UMAirlines.src;

import java.io.*;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * This class responsable for the handling of all the flights present in the system.
 * This class makes a map, in our case, a Treemap where the key is the reservation ID and the value is a Flight.
 */
public class Flights implements Serializable{

    TreeMap<Integer,Flight> fl; 
    public ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

    /**
     * Flights Class Constructor
     */
    public Flights() {
        this.fl = new TreeMap<>();
    }

    /**
     * Flights Class Constructor
     * @param fl New TreeMap of Flights
     */
    public Flights(TreeMap<Integer,Flight> fl) {
        setFlight(fl);
    }

    public Flights(Flights fl) {
        this.fl = fl.getFlight();
    }

    /**
     * Makes a List with Flights
     * @return List containing Flights
     */
    public TreeMap<Integer,Flight> getFlight() {
        TreeMap<Integer,Flight> res = new TreeMap<>();
        for(Map.Entry<Integer,Flight> f : this.fl.entrySet())
            res.put(f.getKey(), f.getValue());
        return res;
    }

    /**
     * Makes a List with the Flight's of a given User
     * @param
     * @return
     */
    public TreeMap<Integer,Flight> getFlightByID(Integer id) {
        TreeMap<Integer,Flight> res = new TreeMap<>();
        for(Map.Entry<Integer,Flight> f : this.fl.entrySet())
            if(f.getValue().getID() == id)
                res.put(f.getKey(), f.getValue());
        return res;
    }

    /**
     * Changes the TreeMap
     * @param fl New TreeMap
     */
    public void setFlight(TreeMap<Integer,Flight> fl) {
        this.fl = new TreeMap<>();
        fl.entrySet().forEach( f -> { this.fl.put(f.getKey(), f.getValue()); });
    }

    /**
     * Adds a new Flight into the TreeMap
     * @param f Flight account to be added
     */
    void addFlight(Flight f) {
        this.fl.put(f.getID(),f);
    }

    void addFlight(String start, String end, Integer capacity) {
        int id = this.fl.size();
        Flight flight = new Flight(id,start,end,capacity);
        this.fl.put(id, flight);
    }

    /**
     * Fetches a Flight account
     * @param id Flight's id 
     * @return Flight
     */
    Flight getFlight(Integer id) {
        Flight f = this.fl.get(id);
        return f;
    }

    /**
     * Checks if a flight exists via it's start and end locations
     * @param start Flight's start location
     * @param end Flight's end location
     * @return <i>True</i> if the flight exists. <i>False</i> if otherwise
     */
    boolean flightExists(String start, String end) {
        for(Map.Entry<Integer, Flight> f : this.fl.entrySet()) {
            if(f.getValue().getStartLocation().equals(start) && f.getValue().getEndLocation().equals(end)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if a flight exists via it's start location, end location and it's capacity
     * @param start Flight's start location
     * @param end Flight's end location
     * @param capacity Flight's capacity
     * @return <i>True</i> if the flight exists. <i>False</i> if otherwise
     */
    boolean flightExists(String start, String end, Integer capacity) {
        for(Map.Entry<Integer, Flight> f : this.fl.entrySet()) {
            if(f.getValue().getStartLocation().equals(start) && f.getValue().getEndLocation().equals(end) && f.getValue().getCapacity().equals(capacity)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Fetches the Flight's id via it's start and end locations
     * @param start Flight's start location
     * @param end Flight's end location
     * @return Flight's id
     */
    Integer getFlightId(String start, String end) {
        Integer f = null;
        if(flightExists(start, end)){
            for(Map.Entry<Integer,Flight> fr : this.fl.entrySet()) {
                if(fr.getValue().exists(start, end)){
                    f = fr.getValue().getID();
                }
            }
        }
        return f;
    }

    /**
     * Conversion of the state of an object into a byte stream
     * @param filepath Path of the file
     * @throws IOException
     */
    public void serialize(String filepath) throws IOException{
        FileOutputStream fos = new FileOutputStream(filepath);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.close();
        fos.close();
    }

    /**
     * Conversion of a byte stream into an object
     * @param filepath Path of the file
     * @return New Flights
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public static Flights deserialize(String filepath) throws IOException, ClassNotFoundException{
        FileInputStream fis = new FileInputStream(filepath);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Flights flights = (Flights) ois.readObject();
        ois.close();
        fis.close();
        return flights;
    }
}
