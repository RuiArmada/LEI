package Util.Exceptions;

@SuppressWarnings("serial")

public class Invalid_Business_Exception extends Exception{
    private String bus_name;
    boolean is_Bus;

    public Invalid_Business_Exception(String name, boolean is_Bus) {
        super();
        this.bus_name = name;
        this.is_Bus = is_Bus;
    }

    public String getBus_Name() {
        return bus_name;
    }

    public boolean isIs_Bus() {
        return is_Bus;
    }

    public void setBus_Name(String name) {
        this.bus_name = name;
    }

    public void setIs_Bus(boolean is_Bus) {
        this.is_Bus = is_Bus;
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("\n The Business named: ");
        s.append(this.bus_name);
        s.append("\nCould not be loaded due to ");
        if(!is_Bus) {
            s.append("\nthe Business: ");
            s.append(this.bus_name);
            s.append("\nbeing Non-Existent");
        }
        return super.toString();
    }
}
