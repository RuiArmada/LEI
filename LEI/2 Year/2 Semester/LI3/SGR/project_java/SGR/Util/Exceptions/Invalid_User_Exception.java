package Util.Exceptions;

@SuppressWarnings("serial")

public class Invalid_User_Exception extends Exception {
    private String user_name;
    boolean is_User;

    public Invalid_User_Exception(String name, boolean is_User) {
        super();
        this.user_name = name;
        this.is_User = is_User;
    }

    public String getUser_Name() {
        return user_name;
    }

    public boolean isIs_User() {
        return is_User;
    }

    public void setUser_Name(String name) {
        this.user_name = name;
    }

    public void setIs_User(boolean is_User) {
        this.is_User = is_User;
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("\n The User named: ");
        s.append(this.user_name);
        s.append("\nCould not be loaded due to ");
        if(!is_User) {
            s.append("\nthe User: ");
            s.append(this.user_name);
            s.append("\nbeing Non-Existent");
        }
        return super.toString();
    }
}
