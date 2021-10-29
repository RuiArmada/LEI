package Model;

import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import Util.NamesBus;

public class TableBusNum {
    private Map<String,Integer> busNumRev;
    private Map<String, NamesBus> difUsers;
            //Business    DifUsers
     /**
     * Empty Constructor for the TableBusNum Class
     */
    public TableBusNum(){
        this.busNumRev = new TreeMap<>();
        this.difUsers = new TreeMap<>();
    } 
/**
     * Constructor for the TableBusNum Class
     * @param map - Map
     * @param set - Map
     */
    public TableBusNum(Map<String,Integer> map, Map<String, NamesBus> set){
        setBusNumRev(map);
        setDifUsers(set);
    }
/**
     * Constructor for the TableBusNum Class
     * @param table - TableBusNum
     */
    public TableBusNum(TableBusNum table){
        setBusNumRev(table.getBusNumRev());
        setDifUsers(table.getDifUsers());
    }
/**
	 * Clones the object TableBusNum
	 * @return TableBusNum
	 */
    public TableBusNum clone(){
        return new TableBusNum(this);
    }
/**
     * Allows access to said variable
     * @return busNumRev - Map
     */
    public Map<String,Integer> getBusNumRev(){
        return this.busNumRev;
    }
/**
     * Allows access to said variable
     * @return difUsers - Map
     */
    public Map<String, NamesBus> getDifUsers(){
        return this.difUsers;
    }
 /**
	 * Transforms the object to a String
	 * @return String
	 */
    public String toString() {    
        StringBuilder sb = new StringBuilder();    
        for(Entry<String,Integer> entry : this.busNumRev.entrySet()){    
            sb.append("\t");    
            sb.append(entry.getKey());    
            sb.append(" : ");    
            sb.append(entry.getValue()); 
            sb.append("\n");  
        }    
        return sb.toString();    
    }
/**
     * Allows the change of said variable
     * @param map - Map
     */
    public void setBusNumRev(Map<String,Integer> map){
        map.entrySet()
                    .forEach(u -> { this.busNumRev.put(u.getKey(), u.getValue());});
    }
 /**
     * Allows the change of said variable
     * @param set - Map
     */
    public void setDifUsers(Map<String, NamesBus> set){
        set.entrySet()
                    .forEach(u -> { this.difUsers.put(u.getKey(), u.getValue().clone());});
    }

    /**
     * Increments Num
     * @param business - String
     */
    public void incrementNum(String business){
        if(this.busNumRev.keySet().contains(business)){
            int newValue = this.busNumRev.get(business);
            newValue++;
            this.busNumRev.replace(business, newValue);
        }
        else{
            this.busNumRev.put(business, 1);
        }
    }
     /**
     * Checks id the business exists
     * @param business - String
     * @return Boolean
     */
    public boolean busExists(String business){
        return this.busNumRev.containsKey(business);
    }
 /**
     * Inserts a Business 
     * @param business - String
     * @param x - Int
     */
    public void addBusiness(String business, int x){
        this.busNumRev.put(business, x);
    }
 /**
     * Inserts a User
     * @param user - String
     * @param business - String
     */
    public void addUser(String user, String business){
        if(this.difUsers.get(business ) != null && !(this.difUsers.get(business).hasString(user))){
            NamesBus namesU = this.difUsers.get(business);
            namesU.addString(user);
            this.difUsers.replace(business, namesU);
            incrementNum(business);
        }
    }
 /**
     * Orders by number of Users
     */
    public void ordByNumUsers(){
        this.busNumRev = this.busNumRev.entrySet().stream()
                            .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                                    (e1, e2) -> e1, LinkedHashMap::new));
    }
}
