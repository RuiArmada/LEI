package Util;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;


public class DataSet {
    private Map<String,Integer> dataSet;
    private Pair<Integer,NamesBus> pair8;

    /**
     * Empty constructor for the DataSet class
     */
    public DataSet(){
        this.dataSet = new HashMap<>();
        this.pair8 = new Pair<Integer, NamesBus>(0,new NamesBus());
    }
    /**
     * Allows access to said variavle
     * @return dataSet - Set
     */
    public Map<String,Integer> getBusSet(){
        return this.dataSet;
    }
    /**
     * Allows access to said variable
     * @return pair8 - Pair
     */
    public Pair<Integer,NamesBus> getBus8(){
        return this.pair8;
    }
    /**
     * Allows the change of said variable
     * @param mapData - Map
     */
    public void setBusSet(Map<String,Integer> mapData){
        this.dataSet = new HashMap<>();
        for(Entry<String,Integer> entry : mapData.entrySet()){
            this.dataSet.put(entry.getKey(), entry.getValue());
        }
    }
    /**
     * Allows the change of said variable
     * @param pair - Pair
     */
    public void setBus8 (Pair<Integer,NamesBus> pair){
        this.pair8 = new Pair<Integer,NamesBus>(pair.getFirst(), pair.getSecond());
    }

    //----------------------------- methods for query 5 -------------------------------------------------

    /**
     * Adds a entry of Business in the DataSet
     * @param revs - Int
     * @param bus - String
     */
    public void addB(int revs, String bus){
        this.dataSet.put(bus,revs);
    }
    
    /**
     * Sorts dataSet by Values
     */
    public void ordDataSet(){
        HashMap<String, Integer> reverseSortedMap = new HashMap<>();
        this.dataSet.entrySet()
            .stream()
            .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder())) 
            .forEachOrdered(x -> reverseSortedMap.put(x.getKey(), x.getValue()));
        setBusSet(reverseSortedMap);
    }

    //----------------------------- methods for query 8 -------------------------------------------------

    /**
     * Adds a Review to a Business
     * @param business - String
     * @return pair8.getFirst() - int
     */
    public int addRev(String business){
        for(String str : this.pair8.getSecond().getSetNames()){
            if(!this.pair8.getSecond().getSetNames().contains(str)){
                int newkey = this.pair8.getFirst();
                newkey++;
                Set<String> set = this.pair8.getSecond().getSetNames();
                set.add(business);
                this.pair8 = new Pair<Integer,NamesBus>(newkey,new NamesBus(set));
            }
        }
           
        return this.pair8.getFirst();
    }

}
