package View.Interfaces;

import Util.Triple;
import Model.Table;

public interface View_SGR_Interface {
    
    void show_pre_menu();
    void show_menu();
    void print_stats();
    void want_back();
    void show_help(String func);
    void helpArgs(String function, String cmd, String args);
    void helpQuery1();
    void helpQuery2();
    void helpQuery3();
    void helpQuery4();
    void helpQuery5();
    void helpQuery6();
    void helpQuery7();
    void helpQuery8();
    void helpQuery9();
    void helpQuery10();
    void ask_friends();
    void ask_defaults();
    void ask_user();
    void ask_business();
    void ask_review();
    void not_loaded();
    void ask_option();
    void show(String s);
    void ask_business_id();
    void ask_year();
    void ask_month();
    void ask_user_id();
    void ask_int_query_6();
    void ask_int_query_8();
    void ask_int_query_9();
    void output_query_2(int year, int month, int counter1, int counter2);
    void benchmark_begin();
    void benchmark_end();
    void printQueryResult(Table t);
    void printTriple(Triple<String,String,String> a);
    
}
