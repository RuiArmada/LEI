package Model;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

public class TableBusClass {
    private Map<String,Float> busClass;
/**
	 * Empty Constructor for the TableBusClass Class
	 */
    public TableBusClass(){
        this.busClass = new HashMap<>();
    }
 /**
     * Constructor for the TableBusClass Class
     * @param map - Map
     */
    public TableBusClass(Map<String,Float> map){
        setBusClass(map);
    }
 /**
     * Constructor for the TableBusClass Class
     * @param table - TableBusClass
     */
    public TableBusClass(TableBusClass table){
        setBusClass(table.getBusClass());
    }
/**
     * Allows access to said variable
     * @return busClass - Map
     */
    public Map<String, Float> getBusClass(){
        return this.busClass;
    }
 /**
     * Allows the change of said variable
     * @param map - Map
     */
    public void setBusClass(Map<String, Float> map){
        map.entrySet()
                    .forEach(u -> { this.busClass.put(u.getKey(), u.getValue());});
    }
 /**
	 * Clones the object TableBusClass
	 * @return TableBusClass
	 */
    public TableBusClass clone(){
        return new TableBusClass(this);
    }
     /**
     * Inserts a Business
     * @param business - String
     * @param medClas - Float
     */
    public void addBusiness(String business, float medClas){
        this.busClass.put(business,medClas);
    }
 /**
	 * Transforms the object to a String
	 * @return String
	 */
    public String toString() {  
        StringBuilder sb = new StringBuilder();  
        for(Entry<String,Float> entry : this.busClass.entrySet()){  
            sb.append("\t");  
            sb.append(entry.getKey());  
            sb.append(" : ");  
            sb.append(entry.getValue());  
            sb.append("\n");  
        }  
        return sb.toString();  
    }
}
