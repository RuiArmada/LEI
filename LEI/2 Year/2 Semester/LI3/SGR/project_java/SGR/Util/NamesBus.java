package Util;

import java.util.TreeSet;
import java.util.Set;

public class NamesBus {
    private Set<String> setNames;

    /**
     * Empty constructor for NamesBus class
     */
    public NamesBus(){
        this.setNames = new TreeSet<>();
    }
    /**
     * Constructor for NamesBus class
     * @param set - Set
     */
    public NamesBus(Set<String> set){
        this.setNames = new TreeSet<>();
        for(String name : set){
            this.setNames.add(name);
        }
    }
    /**
     * Constructor for NamesBus class
     * @param set - NameBus
     */
    public NamesBus(NamesBus set){
        setSetNames(set.getSetNames());
    }
    /**
     * Clones the class
     * @return NamesBus
     */
    public NamesBus clone(){
        return new NamesBus(this);
    }
    /**
     * Allows access to said variable
     * @return setNames - Set
     */
    public Set<String> getSetNames(){
        return this.setNames;
    }
    /**
     * Allows the change of said variable
     * @param set - Set
     */
    public void setSetNames(Set<String> set){
        this.setNames = new TreeSet<>();
        for(String name : set){
            this.setNames.add(name);
        }
    }
    /**
     * Confirms if the String is present
     * @param st - String
     * @return res - Boolean
     */
    public boolean hasString(String st){
        boolean res = false;
        if(this.setNames.contains(st)) res = true;
        return res;
    }   
    /**
     * Adds the String to the Set
     * @param str - String
     */
    public void addString(String str){
        this.setNames.add(str);
    }

}
