package UMAirlines.src;

import java.io.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * This class responsable for the handling of all the reservations present in the system.
 * This class makes a map, in our case, a Treemap where the key is the reservation's id and the value is a Reservation.
 */
public class Reservations implements Serializable{

    AtomicInteger idMaker = new AtomicInteger(69);
    TreeMap<Integer, Reservation> reservations; // ID reserva, Reserva (id é gerado no servidor)
    private List<LocalDate> closed; // List of closed dates
    public ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

    /**
     * Class Contructor
     */
    public Reservations() {
        this.reservations = new TreeMap<>();
        this.closed = new ArrayList<>();
    }

    /**
     * Class Constructor
     * @param reservations New TreeMap of Reservations
     */
    public Reservations(TreeMap<Integer, Reservation> reservations, List<LocalDate> closed) {
        setReservations(reservations);
        setClosed(closed);
    }

    /**
     * Makes a List of Reservations
     * @return List containing Reservations
     */
    public TreeMap<Integer, Reservation> getReservations() {
        TreeMap<Integer, Reservation> ret = new TreeMap<>();
        for(Map.Entry<Integer, Reservation> r : this.reservations.entrySet())
            ret.put(r.getKey(), r.getValue());
        return ret;
    }

    /**
     * Fetches the List of cancelled Flights
     * @return List of closed dates
     */
    public List<LocalDate> getClosed() {
        return (this.closed);
    }

    /**
     * 
     Changes the TreeMap
     * @param res New TreeMap
     */
    public void setReservations(TreeMap<Integer, Reservation> res) {
        this.reservations = new TreeMap<>();
        res.entrySet().forEach( r -> { this.reservations.put(r.getKey(), r.getValue()); });
    }

    /**
     * Changes the List
     * @param closed new List
     */
    public void setClosed(List<LocalDate> closed) {
        this.closed = new ArrayList<>();
        closed.forEach(c -> {this.closed.add(c);}); 
    }

    /**
     * Adds a new reservation into the treemap
     * @param r New Reservation
     * @param i New Reservation's id
     */
    public void addRes(Reservation r, Integer i) {
        this.reservations.put(i, r);
    }

    /**
     * Adds a new reservation into the treemap
     * @param email New User's email address
     * @param id New List of ids of flights
     * @param date New Flight's date 
     */
    public void addRes(String email, List<Integer> id, LocalDate date) {
        this.reservations = new TreeMap<>();
        for(Map.Entry<Integer, Reservation> r : this.reservations.entrySet()) {
            r.getValue().setuEmail(email);
            r.getValue().setIdF(id);
            r.getValue().setFlightDate(date);
        }
    }

    /**
     * Adss a new date of closure into the list
     * @param date New Date of closure
     */
    public void addClosed(LocalDate date) {
        this.closed.add(date);
    }

    /**
     * Checks if the flight's capacity permits the addiction of a reservation.
     * @param idf Flight's id
     * @param date Flight's date
     * @param maxCap Flight's max capacity
     * @return <i>True</i> if the flight capacity is at it's max value. <i>False</i> if otherwise
     */
    boolean verifyCapacity(int idf, LocalDate date, int maxCap) {
        int cap = 0;
        for(Map.Entry<Integer, Reservation> r : this.reservations.entrySet()) {
            Reservation res = r.getValue();
            if(res.getIdF().contains(idf) && res.getFlightDate().equals(date)) {
                cap++;
            }
        }
        if(cap >= maxCap) return false;
        return true;
    }

    /**
     * Checks if the date is not closed.
     * @param date Date to be checked
     * @return <i>True</i> if the date is not closed. <i>False</i> if otherwise
     */
    boolean dateNotClosed(LocalDate date) {
        if(closed.contains(date)) return false;
        else return true;
    }

    /**
     * Checks if the reservation exists.
     * @param id Reservation's id
     * @return <i>True</i> if the reservation exists. <i>False</i> if otherwise
     */
    boolean reserVationExists(Integer id) {
        return this.reservations.containsKey(id);
    }

    /**
     * Checks if the reservation of a given user exists
     * @param id Reservation's id
     * @param user User's email address
     * @return <i>True</i> if the reservation exists. <i>False</i> if otherwise
     */
    boolean fromUser(Integer id, String user) {
        Reservation res = this.reservations.get(id);
        return res.getuEmail().equals(user);
    }

    /**
     * Eliminates a reservation
     * @param id Reservation's id
     */
    void removeReservation(Integer id) {
        this.reservations.remove(id);
    }

    /**
     * Conversion of the state of an object into a byte stream
     * @param filepath Path of the file
     * @throws IOException
     */
    public void serialize(String filepath) throws IOException {
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
    public static Reservations deserialize(String filepath) throws IOException, ClassNotFoundException {
        FileInputStream fis = new FileInputStream(filepath);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Reservations reservations = (Reservations) ois.readObject();
        ois.close();
        fis.close();
        return reservations;
    }
}
