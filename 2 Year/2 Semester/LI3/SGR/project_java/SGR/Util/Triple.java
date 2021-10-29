package Util;

public class Triple<T, U, V> extends Object{

    private T first;
    private U second;
    private V third;

    /**
    * Constructor for the class Pair
    * @param first - Any Type
    * @param second - Any Type
    * @param third - Any Type
    */
    public Triple(T first, U second, V third) {
        this.first = first;
        this.second = second;
        this.third = third;
    }
    /**
    * Allows access to said variable
    * @return first - Any Type 
    */
    public T getFirst() { return first; }
    /**
    * Allows access to said variable
    * @return second - Any Type 
    */
    public U getSecond() { return second; }
    /**
    * Allows access to said variable
    * @return third - Any Type 
    */
    public V getThird() { return third; }
    /**
    * Allows the change of said variable
    * @param first - Any Type
    */
    public void setFirst(T first) { this.first = first; }
    /**
    * Allows the change of said variable
    * @param second - Any Type
    */
    public void setSecond(U second) { this.second = second; }
    /**
    * Allows the change of said variable
    * @param third - Any Type
    */
    public void setThird(V third) { this.third = third; }
    /**
    * Transforms the type into a String
    * @return string - String 
    */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("First: ");
        sb.append(this.first);
        sb.append("     ");
        sb.append("Second: ");
        sb.append(this.second);
        sb.append("     ");
        sb.append("Third: ");
        sb.append(this.third);
        return super.toString();
    }

}
