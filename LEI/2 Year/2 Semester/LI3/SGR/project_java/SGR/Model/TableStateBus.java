package Model;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

public class TableStateBus {
    private Map<String,TableBusClass> stateBus;
/**
     * Empty Constructor for the TableStateBus Class
     */
    public TableStateBus(){
        this.stateBus = new HashMap<>();
    }
/**
     * Constructor for the TableStateBus Class
     * @param map - Map
     */
    public TableStateBus(Map<String,TableBusClass> map){
        setBusClass(map);
    }
/**
     * Constructor for the TableStateBus Class
     * @param table - TableStateBus
     */
    public TableStateBus(TableStateBus table){
        setBusClass(table.getBusClass());
    }
/**
     * Allows access to said variable
     * @return stateBus - Map
     */
    public Map<String,TableBusClass> getBusClass(){
        return this.stateBus;
    }
 /**
     * Allows the change of said variable
     * @param map - Map
     */
    public void setBusClass(Map<String,TableBusClass> map){
        map.entrySet()
                    .forEach(u -> { this.stateBus.put(u.getKey(), u.getValue().clone());});
    }
 /**
	 * Clones the object TableStateBus
	 * @return TableStateBus
	 */
    public TableStateBus clone(){
        return new TableStateBus(this);
    }
 /**
     * Inserts a State
     * @param state - String
     * @param business - String
     * @param medClas - Float
     */
    public void addState(String state, String business, float medClas){
        
        if(this.stateBus.containsKey(state)){
           
            Map<String,Float> table = this.stateBus.get(state).getBusClass();
            table.put(business, medClas);
            this.stateBus.get(state).setBusClass(table);

        } else {
            TableBusClass table = new TableBusClass();
            table.addBusiness(business, medClas);
            this.stateBus.put(state, table);
        }
    }
 /**
	 * Transforms the object to a String
	 * @return String
	 */
    public String toString() {  
        StringBuilder sb = new StringBuilder();  
        for(Entry<String,TableBusClass> entry : this.stateBus.entrySet()){  
            sb.append("\t");  
            sb.append(entry.getKey());  
            sb.append(" : "); 
            String table = entry.getValue().toString(); 
            sb.append(table);  
            sb.append("\n");  
        }  
        return sb.toString();  
    }
}
