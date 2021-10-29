package Model.Interfaces;

import java.util.Map;
import java.util.Set;

import Util.Triple;
import Model.Table;

public interface Table_Interface {
    
    public Table clone();
    public boolean equals(Object obj);
    public Set<String> getCodNames();
    public int getQueryN();
    public int getCounter1();
    public int getCounter2();
    public Map<String, Triple<Integer, Integer, Float>> getInfo();
    public Map<Integer, Map<String, Integer>> getInfo2();
    public Map<String, Triple<String, String, String>> getInfo3();
    public Map<String, Integer> getInfo4();
    public Map<String, Float> getInfo5();
    public Map<String, Map<String, Map<String, Float>>> getInfo6();
    public void setCodNames(Set<String> codNames);
    public void setQueryN(int queryN);
    public void setCounter1(int counter1);
    public void setCounter2(int counter2);
    public void setInfo(Map<String, Triple<Integer, Integer, Float>> info);
    public void setInfo2(Map<Integer, Map<String, Integer>> info2);
    public void setInfo3(Map<String, Triple<String, String, String>> info3);
    public void setInfo4(Map<String, Integer> info4);
    public void setInfo5(Map<String, Float> info5);
    public void setInfo6(Map<String, Map<String, Map<String, Float>>> info6);

}
