package Model;

import Util.Triple;

import java.io.Serializable;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class Table implements Serializable{
    private static final long serialVersionUID = 1656334331841663697L;

    private int queryN; //todas
    private Set<String> codNames; //query 1
    private int counter1; // query 1 e 2
    private int counter2; // query 2
    private Map<Integer, Triple<Integer,Integer,Float>> info; // query 3 e 4
    private Map<Integer, HashMap<String,Integer>> info2; // query 6
    private Map<String, Map<String,Integer>> info3; // query 7
    private Map<String, Integer> info4; //query 8 e 5
    private Map<String, Float> info5; //query 9
    private Map<String, TableStateBus> info6; //query10
    //          Estado      Cidade     Busiiness, Classificacao         //
     /**
     * Empty Constructor for the Table Class
     */
    public Table(){
        this.queryN = 0;
        this.codNames = new TreeSet<>();
        this.counter1 = 0;
        this.counter2 = 0;
        this.info = new TreeMap<>();
        this.info2 = new HashMap<>();
        this.info3 = new TreeMap<>();
        this.info4 = new HashMap<>();
        this.info5 = new TreeMap<>();
        this.info6 = new TreeMap<>();
        }
/**
     * Constructor for the Table Class
     * @param queryN - Int
     * @param codNames - Set
     * @param counter1 - Int
     * @param counter2 - Int
     * @param info - Map
     * @param info2 - Map
     * @param info3 - Map
     * @param info4 - Map
     * @param info5 -  Map
     * @param info6 - Map
     */
    public Table(int queryN, Set<String> codNames,int counter1, int counter2, Map<Integer, Triple<Integer,Integer,Float>> info,
                Map<Integer, HashMap<String,Integer>> info2, Map<String, Map<String,Integer>> info3, Map<String, Integer> info4,
                Map<String, Float> info5, Map<String, TableStateBus> info6)
    {
        this.codNames = codNames;
        this.queryN = queryN;
        this.counter1 = counter1;
        this.counter2 = counter2;
        this.info = info;
        this.info2 = info2;
        this.info3 = info3;
        this.info4 = info4;
        this.info5 = info5;
        this.info6 = info6;
    }
 /**
     * Constructor for the Table Class
     * @param t - Table
     */
    public Table(Table t){
        this.codNames = t.getCodNames();
        this.queryN = t.getQueryN();
        this.counter1 = t.getCounter1();
        this.counter2 = t.getCounter2();
        this.info = t.getInfo();
        this.info2 = t.getInfo2();
        this.info3 = t.getInfo3();
        this.info4 = t.getInfo4();
        this.info5 = t.getInfo5();
        this.info6 = t.getInfo6();

    }
  /**
	 * Clones the object Table
	 * @return Table
	 */
    public Table clone() {
        return new Table(this);
    }
    
    /**
	 * Method that compares one Object to another
	 * @return Boolean
	 */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Table t1 = (Table) obj;
        return t1.getQueryN() == this.queryN &&
               t1.getCodNames() == this.codNames &&
               t1.getCounter1() == this.counter1 &&
               t1.getCounter2() == this.counter2 &&
               t1.getInfo().equals(this.info) &&
               t1.getInfo2().equals(this.info2) &&
               t1.getInfo3().equals(this.info3) &&
               t1.getInfo4().equals(this.info4) &&
               t1.getInfo5().equals(this.info5) &&
               t1.getInfo6().equals(this.info6);
               
    }
 /**
     * Checks if the Table is Empty
     * @return Boolean
     */
    public boolean isEmpty() {
        return getQueryN() == 0 &&
               getCodNames().size() == 0 && 
               getCounter1() == 0 &&
               getCounter2() == 0 &&
               getInfo().size() == 0 &&
               getInfo2().size() == 0 &&
               getInfo3().size() == 0 &&
               getInfo4().size() == 0 &&
               getInfo5().size() == 0 &&
               getInfo6().size() == 0;
    }
    
     //----------------------------- gets e sets ------------------------------------------

     /**
      * Allows access to said variable
      * @return codNames - Set
      */
     public Set<String> getCodNames() {
         return this.codNames;
     }
 /**
      * Allows access to said variable
      * @return queryN - Int
      */
     public int getQueryN() {
         return this.queryN;
     }
 /**
      * Allows access to said variable
      * @return counter1 - Int
      */
     public int getCounter1() {
         return this.counter1;
     }
 /**
      * Allows access to said variable
      * @return counter2 - Int
      */
     public int getCounter2() {
         return this.counter2;
     }
 /**
      * Allows access to said variable
      * @return info - Map
      */
     public Map<Integer, Triple<Integer, Integer, Float>> getInfo() {
         return this.info;
     }
      /**
      * Allows access to said variable
      * @return info2 - Map
      */
     public Map<Integer, HashMap<String,Integer>> getInfo2() {
         return this.info2;
     }
      /**
      * Allows access to said variable
      * @return info3 - Map
      */
     public Map<String, Map<String,Integer>> getInfo3() {
        return this.info3;
    }
    /**
      * Allows access to said variable
      * @return info4 - Map
      */
     public Map<String, Integer> getInfo4() {
         return this.info4;
     }
      /**
      * Allows access to said variable
      * @return info5 - Map
      */
     public Map<String, Float> getInfo5() {
         return this.info5;
     }
      /**
      * Allows access to said variable
      * @return info6 - Map
      */
     public Map<String, TableStateBus> getInfo6() {
         return this.info6;
     }
     /**
      * Allows the change of said variable
      * @param codNames - Set
      */
     public void setCodNames(Set<String> codNames) {
        Set<String> set = new TreeSet<>();
        this.codNames = set;
        for (String string : codNames) {
            this.codNames.add(string);
        }
     }
/**
      * Allows the change of said variable
      * @param queryN - Int
      */
     public void setQueryN(int queryN) {
         this.queryN = queryN;
     }
 /**
      * Allows the change of said variable
      * @param counter1 - Int
      */
     public void setCounter1(int counter1) {
         this.counter1 = counter1;
     }
/**
      * Allows the change of said variable
      * @param counter2 - Int
      */
     public void setCounter2(int counter2) {
         this.counter2 = counter2;
     }
/**
      * Allows the change of said variable
      * @param info - Map
      */
     public void setInfo(Map<Integer, Triple<Integer, Integer, Float>> info) {
        info.entrySet()
        .forEach(u -> { this.info.put(u.getKey(), u.getValue());});
     }
 /**
      * Allows the change of said variable
      * @param info2 - Map
      */
     public void setInfo2(Map<Integer, HashMap<String,Integer>> info2) {
        info2.entrySet().forEach(u -> { this.info2.put(u.getKey(), u.getValue());});
     }
/**
      * Allows the change of said variable
      * @param info3 - Map
      */
     public void setInfo3(Map<String, Map<String,Integer>> info3) {
        info3.entrySet()
        .forEach(u -> { this.info3.put(u.getKey(), u.getValue());});
     
     }
/**
      * Allows the change of said variable
      * @param info4 - Map
      */
     public void setInfo4(Map<String, Integer> info4) {
        info4.entrySet()
        .forEach(u -> { this.info4.put(u.getKey(), u.getValue());});
     
     }
 /**
      * Allows the change of said variable
      * @param info5 - Map
      */
     public void setInfo5(Map<String, Float> info5) {
        info5.entrySet()
        .forEach(u -> { this.info5.put(u.getKey(), u.getValue());});
     }
/**
      * Allows the change of said variable
      * @param info6 - Map
      */
     public void setInfo6(Map<String, TableStateBus> info6) {
        info6.entrySet()
        .forEach(u -> { this.info6.put(u.getKey(), u.getValue());});
     
     }

    //---------------------------------------- queries methods -------------------------------------------------------
/**
     * Adds a codName
     * @param str - String
     */
    public void addCodeName(String str){
        this.codNames.add(str);
    }
 /**
     * Adds a EntryInfo
     * @param num - Int
     * @param triple - Triple
     */
    public void addEntryInfo(int num, Triple<Integer,Integer,Float> triple){
        this.info.put(num, triple);
    }
/**
     * Adds a EntryInfo4
     * @param str - String
     * @param num - Int
     */
    public void addEntryInfo4(String str,int num){
        this.info4.put(str, num);
    }
  /**
     * Access the Value of Info4
     * @param str - String
     * @return Int
     */
    public int getValueInfo4(String str){
       return this.info4.get(str);
    }
 /**
     * Checks size of Info4
     * @return Int
     */
    public int sizeInfo4(){
        return this.info4.size();
    }
 /**
     * Sorts Info4
     */
    public void sortInfo4(){
       this.info4 = this.info4.entrySet().stream()
                       .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                       .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                               (e1, e2) -> e1, LinkedHashMap::new));
    }
 /**
     * Adds EntryInfo2
     * @param year - Int
     * @param table - TableBusNum
     */
    public void putEntryInfo2(int year, HashMap<String,Integer>table){
       this.info2.put(year, table);
    }
/**
     * Adds EntryInfo3
     * @param str - String
     * @param mapData - Map
     */
    public void putEntryInfo3(String str, Map<String,Integer> mapData){
        this.info3.put(str, mapData);
     }
 /**
     * Checks if contains User Id
     * @param id - String
     * @return Boolean
     */
    public boolean containsUserID(String id){
        return this.info4.containsKey(id);
    }
/**
     * Replaces the id in Value
     * @param ID - String
     * @param num - Int
     */
    public void replaceIDvalue(String ID, int num){
        this.info4.replace(ID, num);
    }
 /**
     * Checks if City is in Info6
     * @param city - String
     * @return Boolean
     */
    public boolean containsCityInfo6(String city){
        return this.info6.containsKey(city);
    }
 /**
     * Access the value of Info6
     * @param city - String
     * @return TableStateBus
     */
    public TableStateBus getValueInfo6(String city){
        return this.info6.get(city);
    }
 /**
     * Adds EntryInfo6
     * @param str - String
     * @param table - TableStateBus
     */
    public void addEntryInfo6(String str, TableStateBus table){
        this.info6.put(str, table);
    }
}
