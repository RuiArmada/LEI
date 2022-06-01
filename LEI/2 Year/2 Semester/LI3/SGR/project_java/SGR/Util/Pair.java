package Util;

public class Pair<T,U> extends Object{
        private T first;
        private U second;

        /**
         * Constructor for the class Pair
         * @param first - Any Type
         * @param second - Any Type
         */
        public Pair(T first, U second) {
            this.first = first;
            this.second = second;
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
         * Allows the change of said variable
         * @param first - Any Type
         */
        public void setFirst(T first) { this.first = first; }
        /**
         * Allows the change of said variable
         * @param second - Any Type
         */
        public void setSecond(U second) { this.second = second; }
        
}

