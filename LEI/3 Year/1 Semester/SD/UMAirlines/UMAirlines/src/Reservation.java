package UMAirlines.src;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * This class is responsible for all the reservation information that is present in the system.
 * In our case a reservation is composed by an User email address, a list of integers containg ids of flights and the date of the reservation's flight.
 */
public class Reservation implements Serializable {

    private String uEmail; // Email of the User that the reservation belongs
    private List<Integer> idF; //flight id
    private LocalDate flightDate; //flightDate

    /**
     * Class Constructor
     */
    public Reservation() {
        this.uEmail = null;
        this.idF = new ArrayList<>();
        this.flightDate = null;
    }

    /**
     * Class Constructor
     * @param uEmail Client's email address
     * @param idF Flight's id
     * @param flightDate Flight's date of departure
     * @param cancelled List of cancelled flights dates
     */
    public Reservation(String uEmail, List<Integer> idF, LocalDate flightDate) {
        this.uEmail = uEmail;
        this.idF = idF;
        this.flightDate = flightDate;
    }

    /**
     * Class Constructor
     * @param reservation Reservation
     */
    public Reservation(Reservation reservation) {
        this.uEmail = reservation.getuEmail();
        this.idF = reservation.getIdF();
        this.flightDate = reservation.getFlightDate();
    }

    /**
     * Fetches the Client's email address
     * @return Client's email address
     */
    public String getuEmail() {
        return this.uEmail;
    }

    /**
     * Fetches the Flight's id
     * @return Flight's id
     */
    public List<Integer> getIdF() {
        return (this.idF);
    }

    /**
     * Fetches the Flight's date of departure
     * @return Flight's date of departure
     */
    public LocalDate getFlightDate() {
        return this.flightDate;
    }


    /**
     * Changes the User's email address
     * @param uEmail new ewmail address
     */
    public void setuEmail(String uEmail) {
        this.uEmail = uEmail;
    }

    /**
     * Changes the ID
     * @param idF new ID
     */
    public void setIdF(List<Integer> idF) {
        this.idF = new ArrayList<>();
        idF.forEach(i ->{this.idF.add(i);});
    }

    /**
     * Changes the Date
     * @param flightDate new Date
     */
    public void setFlightDate(LocalDate flightDate) {
        this.flightDate = flightDate;
    }


    /**
     * Compares an object to another
     * @param obj Object to be compared with
     * @return <i>True</i> if the objects are equal. <i>False</i> if otherwise
     */
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (obj == null || obj.getClass() != this.getClass())
            return false;
        Reservation res = (Reservation) obj;
        return Objects.equals(this.uEmail, res.getuEmail())
                &&
                Objects.equals(this.idF, res.getIdF())
                &&
                Objects.equals(this.flightDate, res.getFlightDate());
    }

    /**
     * Turns a Object into the string format
     * @return String
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("User email: ");
        sb.append(this.uEmail);
        sb.append("\nFlight id list: ");
        sb.append(this.idF.toString());
        sb.append("\nFlight date: ");
        sb.append(this.flightDate);
        return sb.toString();
    }

}
