package UMAirlines.src;

import java.io.*;
import java.util.Objects;

/**
 * This class is responsible for all the flight information that is present in the system.
 * In our case a flight is composed by an id, a start location, the end location and the flight's capacity.
 */
public class Flight implements Serializable {

    private Integer fID; // Id of flight
    private String start; // Position where the flight starts
    private String end; // Location where the flight ends
    private Integer capacity; //Maximum capacity of flight

    /**
     * Class Constructor
     */
    public Flight() {
        this.fID = null;
        this.start = null;
        this.end = null;
        this.capacity = null;
    }

    /**
     * Class Constructor
     * @param fID Flight's Id
     * @param start Flight's start location
     * @param end Flight's end location
     * @param capacity Flight's capacity
     */
    public Flight(Integer fID, String start, String end, Integer capacity) {
        this.fID = fID;
        this.start = start;
        this.end = end;
        this.capacity = capacity;
    }

    /**
     * Class Constructor
     * @param f Flight
     */
    public Flight(Flight f) {
        this.fID = f.getID();
        this.start = f.getStartLocation();
        this.end = f.getEndLocation();
        this.capacity = f.getCapacity();
    }

    /**
     * Fetches the Flight's ID
     * @return Flight's id
     */
    public Integer getID() {
        return this.fID;
    }

    /**
     * Fetches the Flight's start location
     * @return Flight's start locale
     */
    public String getStartLocation() {
        return this.start;
    }

    /**
     * Fetches the Flight's end location
     * @return Flight's end locale
     */
    public String getEndLocation() {
        return this.end;
    }

    /**
     * Fetches the flight capacity
     * @return Flight's capacity
     */
    public Integer getCapacity() {
        return this.capacity;
    }

    /**
     * Changes the Flight Id
     * @param fID Flight's id
     */
    public void setID(Integer fID) {
        this.fID = fID;
    }

    /**
     * Changes the Flight's start location
     * @param start Flight's start locale
     */
    public void setStartLocation(String start) {
        this.start = start;
    }

    /**
     * Changes the Flight's end location
     * @param end Flight's end locale
     */
    public void setEndLocation(String end) {
        this.end = end;
    }

    /**
     * Changes the flight capacity
     * @param capacity Flight's Capacity
     */
    public void setCapacity(Integer capacity) {
        this.capacity = capacity;
    }

    /**
     * Checks if a Flight exists via using the flight id
     * @param id Flight's id
     * @return <i>True</i> if the Flight exists. <i>False</i> if otherwise
     */
    public boolean exists(Integer id) {
        if(!(getID() == id))
            return false;
        else
            return true;
    }

    /**
     * Check if a Flight exists via using the start and end locations
     * @param orig Flight's start location
     * @param dest Flight's end location
     * @return <i>True</i> if the Flight exists. <i>False</i> if otherwise
     */
    public boolean exists(String orig, String dest) {
        if(this.start.equals(orig) && this.end.equals(dest))
            return true;
        return false;
    }

    /**
     * Compares an object to another
     * @param obj Object to be compared with
     * @return <i>True</i> if the objects are equal. <i>False</i> if otherwise
     */
    public boolean equals(Object obj) {
        if(obj == this)
            return true;
        if(obj == null || obj.getClass() != this.getClass())
            return false;
        Flight f = (Flight) obj;
        return Objects.equals(fID, f.getID())
               &&
               Objects.equals(start, f.getStartLocation())
               &&
               Objects.equals(end, f.getEndLocation())
               &&
               Objects.equals(capacity, f.getCapacity());
    }

    /**
     * Turns a Object into the string format
     * @return String
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nStart Location: ");
        sb.append(start);
        sb.append("\nEnd Location: ");
        sb.append(end);
        sb.append("\nFlight capacity: ");
        sb.append(capacity);
        return sb.toString();
    }

     /**
     * Conversion of the state of an object into a byte stream
     * @param path Path of the file
     * @throws IOException
     */
    public void serialize(String path) throws IOException {
        FileOutputStream output = new FileOutputStream(path);
        ObjectOutputStream obj = new ObjectOutputStream(output);
        obj.writeObject(this);
        obj.close();
        output.close();
    }

    /**
     * Conversion of a byte stream into an object
     * @param path Path of the file
     * @return New Flight
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public static Flight deserialize(String path) throws IOException, ClassNotFoundException {
        FileInputStream input = new FileInputStream(path);
        ObjectInputStream obj = new ObjectInputStream(input);
        Flight f = (Flight) obj.readObject();
        obj.close();
        input.close();
        return f;
    }
    
}
