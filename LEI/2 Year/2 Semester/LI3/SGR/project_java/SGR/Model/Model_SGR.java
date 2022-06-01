package Model;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.stream.Collectors;


import Util.Triple;
import Util.DataSet;
import Util.Exceptions.Invalid_Review_Exception;

public class Model_SGR {
    private static final long serialVersionUID = 916265096638002828L;

    private Review_cat reviews;
    private User_cat users;
    private Business_cat businesses;
    private Stats stats;
  /**
     * Empty Constructor for the Model_SGR Class
    */
    
    public Model_SGR(){
        this.reviews = new Review_cat();
        this.users = new User_cat();
        this.businesses = new Business_cat();
	this.stats = new Stats();
    }
/**
     * Constructor for the Model_SGR Class
     * @param reviews - Reviews_cat
     * @param users - User_cat
     * @param businesses - Business_cat
     * @param stats - Stats
     */
    public Model_SGR(Review_cat reviews, User_cat users, Business_cat businesses, Stats stats){
        this.reviews = reviews;
        this.users = users;
        this.businesses = businesses;
	this.stats = stats;
    }
/**
     * Allows access to said variable
     * @return businesses - Business_cat
     */
    public Business_cat getBusinesses() {
        return this.businesses;
    }
  /**
     * Allows access to said variable
     * @return reviews - Review_cat
     */
    public Review_cat getReviews() {
        return this.reviews;
    }
    /**
     * Allows access to said variable
     * @return user - User_cat
     */
    public User_cat getUsers() {
        return this.users;
    }
    
    public Stats getStats() {
        return this.stats;
    }
 
    public void setUsers(User_cat users){
        this.users = new User_cat(users);
    }
    /**
     * Allows the change of said variable
     * @param businesses - Business_cat
     */
    public void setBusinesses(Business_cat businesses){
        this.businesses = new Business_cat(businesses);
    }
    /**
     * Allows the change of said variable
     * @param revs - Review_cat
     */
    public void setReviews(Review_cat revs){
        this.reviews = new Review_cat(revs);
    }
    /**
     * Allows the change of said variable
     * @param stats - Stats
     */
    public void setStats(Stats stats){
        this.stats = new Stats(stats);
    }

    //------------------------ methods for file saving -----------------------------
/**
     * Saves the state of the App in a object file
     * @param file - String
     * @throws IOException - Exception
     */
    public void save(String file) throws IOException {
        FileOutputStream a = new FileOutputStream(file);
        ObjectOutputStream r = new ObjectOutputStream(a);
        r.writeObject(this);
        r.flush();
        r.close();
    }

/**
     * Loads the state of the program from a object file
     * @param file - String
     * @return u - Model_SGR
     * @throws IOException - Exception
     * @throws ClassNotFoundException - Exception
     */
    public Model_SGR read(String file)throws IOException ,ClassNotFoundException {
        FileInputStream r = new FileInputStream(file);
        ObjectInputStream a = new ObjectInputStream(r);
        Model_SGR u = (Model_SGR) a.readObject();
        a.close();
        return u;
    }

    //------------------------ methods for parsing ----------------------------------

 /**
     * Adds User
     * @param id - String
     * @param name - String
     */
    public void addUser(String id, String name){
        User newUser = new User(name,0);
        this.users.put(id, newUser);
	this.users.increment_n();
	stats.setNumber_users(stats.getNumber_users()+1);
    }
/**
     * Adds User
     * @param id - String
     * @param name - String
     * @param friends - Set
     */
    public void addUser(String id, String name, Set<String> friends){
        User newUser = new User(name, friends, 0);
        this.users.put(id, newUser);
	this.users.increment_n();
	stats.setNumber_users(stats.getNumber_users()+1);
    }
/**
     * Add Business
     * @param id -  String
     * @param name - String
     * @param city - String
     * @param state - String
     * @param categories - Set
     */
    public void add_business(String id, String name, String city, String state, Set<String> categories){
        Business new_business = new Business(name,city,state,categories,0,0);
        this.businesses.put(id, new_business);
	this.businesses.increment_n();
	stats.setNumber_businesses(stats.getNumber_businesses()+1);
    }
    /**
     * Adds Review
     * @param r_id - String
     * @param u_id - String
     * @param b_id - String
     * @param stars - Float
     * @param useful - Int
     * @param funny - Int
     * @param cool - Int
     * @param date - Date
     * @param text - String
     * @throws Invalid_Review_Exception - Exception
     */
    public void add_review(String r_id, String u_id, String b_id, float stars, int useful, int funny, int cool, Date date, String text) throws Invalid_Review_Exception{
        Review new_review = new Review(u_id,b_id,stars,useful,funny,cool,date,text);
        boolean exists_user = this.users.contains(u_id);
        boolean exists_business = this.businesses.contains(b_id);
        if(exists_user && exists_business){
            User user = this.users.getU_catalog().get(u_id);
		    user.plusRev();
	    	Business bus = businesses.get_b_catalog().get(b_id);
		    float nstars = (bus.get_m_stars() + stars)/2;
	    	bus.plusRev();
	    	bus.set_m_stars(nstars);
	    	this.reviews.put(r_id, new_review);
	    	this.reviews.increment();
	    	stats.setNumber_reviews(stats.getNumber_reviews()+1);
            if(useful+cool+funny == 0){
                stats.setNull_reviews(stats.getNull_reviews()+1);
            }
        }
        else{
            stats.setWrong_reviews(stats.getWrong_reviews()+1);
            throw new Invalid_Review_Exception(u_id,b_id,r_id,exists_business,exists_user);
        }
    }

    //------------------------Stats-----------------------------------
    /**
     * Gets Stats of User in the Catalog
     */
    public void get_stats_users_in_cat(){
	    Collection<User> value = this.users.getU_catalog().values();
	    int y=0,n=0; 
	    for (User e : value)
		    if(e.getN_reviews() == 0)
			    n++;
		    else
			    y++;
	    this.stats.setNumber_users_with_reviews(y);
	    this.stats.setNumber_users_with_no_reviews(n);
    }
     /**
     * Gets Stats of Reviewed Businesses in the Catalog
     */
    public void get_stats_reviewed_businesses_in_cat(){
	    Collection<Business> value = this.businesses.get_b_catalog().values();
	    int y=0,n=0; 
	    for (Business e : value)
		    if(e.get_n_reviews() == 0)
			    n++;
		    else
			    y++;
	    this.stats.setNumber_non_reviewed_businesses(n);
	    this.stats.setNumber_reviewed_businesses(y);
    }
/**
     * Gets per month Stats
     */
    public void get_per_month_info(){
	    Collection<Review> rev = this.reviews.getR_catalog().values();
	    Map<Integer,Integer> out_1 = new HashMap<>();
	    Map<Integer,Float> out_2 = new HashMap<>();
	    Map<Integer,HashSet<String>> out_3 = new HashMap<>();
	    rev.forEach(u->{
		    int k = u.getDate().getMonth();
		    if(out_1.get(k)!=null){
			    out_1.put(k, out_1.get(k)+1);
		    }else
			    out_1.put(k, 1);

		    if(out_2.get(k)!=null){
			    out_2.put(k,(out_2.get(k)+u.getStars())/2);
		    }else
			    out_2.put(k, u.getStars());

		    if(out_3.get(k)!=null){
			    HashSet<String> l = out_3.get(k);
				l.add(u.get_user_id());
			    out_3.put(k,l);
		    }else{
			    HashSet<String> l = new HashSet<>();
			    l.add(u.get_user_id());
			    out_3.put(k,l);
		    }
	    });
	    Collection<Float> average = out_2.values();
	    float global = 0;
	    for(float f : average){
		    global += f;
	    }
	    global = global/average.size();
	    Map<Integer,Integer> urs = new HashMap<>();
	    out_3.entrySet().forEach(u->{urs.put(u.getKey(),u.getValue().size());});
	    this.stats.setReviews_per_month(out_1);
	    this.stats.setAverage_per_month(out_2);
	    this.stats.setUsers_per_month(urs);
	    this.stats.setGlobal_average(global);
    }
/**
     * Sets the Stats of the Files
     */
    public void set_files_stats(){
	    Triple<String,String,String> files = new Triple<String,String,String>("users_full.csv","business_full.csv","reviews_1M.csv");
	    this.stats.setFiles(files);
    }
    /**
     * Sets the Stats of the Files
     * @param use - String
     * @param bus - String
     * @param rev - String
     */
    public void set_files_stats(String use,String bus,String rev){
	    Triple<String,String,String> files = new Triple<String,String,String>(use,bus,rev);
	    this.stats.setFiles(files);
    }


    //------------------------queries----------------------------------
 /**
     * Ordered list of never reviewed businesses and how many there are in total.
     * @return Table
     */
    public Table query1(){
	    Table t = new Table();
        int counter = 0;
        t.setQueryN(1);
        
        for(Entry<String,Business> entry : this.businesses.get_b_catalog().entrySet()) {
            if (entry.getValue().get_n_reviews() == 0) {
                t.addCodeName(entry.getKey());
                counter++;
            }
        }
        t.setCounter1(counter);
	    return t;
    }

/**
     * Total number of reviews by different users in a specific month and year.
     * @param year - Int
     * @param month - Int
     * @return Table
     */

    public Table query2(int year, int month){
	    Table t = new Table();
        t.setQueryN(2);
        int counter1 = 0; //num reviews
        int counter2 = 0; //num users
        List<String> users = new ArrayList<>();
        if(month < 1 || month > 12 || year > 2021 || year < 2000) {
            return t;
        }
        for(Entry<String,Review> entry : this.reviews.getR_catalog().entrySet()) {
            int cenas = entry.getValue().getYearR();
            if (cenas == year && entry.getValue().getMonthR() == month) {
                counter1++;
                
                if (!users.contains(entry.getValue().get_user_id())){
                    users.add(entry.getValue().get_user_id());
                    counter2++;
                }
            }   
        }
        t.setCounter1(counter1);
        t.setCounter2(counter2);


	    return t; 
    }
      /**
     * User stats per month.
     * @param user_id - String
     * @return Table
     */
    public Table query3(String user_id){ //needs user doesnt exist exception
        Table t = new Table();
        t.setQueryN(3);
        
        int[][] revs = new int[12][2]; // numReviews, nNegTotais
        float[] cl = new float[12]; // classificacao
        int[] nNegDist = new int[12]; //negocios distintos
        int n = 0;
        for(int i = 0; i<12; i++){
            cl[i] = 0;
            nNegDist[i] = 0;
            for(int j = 0; j<2; j++){
                revs[i][j] = 0;
            }
        }
        Map<Integer, List<String>> bus = new TreeMap<>();
        while(n<12){
            bus.put(n, new ArrayList<>());
            n++;
        }

        for (Entry<String,Review> entry : this.reviews.getR_catalog().entrySet()) {
            if(entry.getValue().get_user_id().equals(user_id)){
                revs[entry.getValue().getMonthR()-1][0]++;
                revs[entry.getValue().getMonthR()-1][1]++;
                cl[entry.getValue().getMonthR()-1]+=entry.getValue().getStars();
                
                if(!(bus.get(entry.getValue().getMonthR()).contains(entry.getValue().getBusiness_id()))
                    && bus.get(entry.getValue().getMonthR())!=null){
                    bus.get(entry.getValue().getMonthR()).add(entry.getValue().getBusiness_id());
                    nNegDist[entry.getValue().getMonthR()-1]++;

                }
            }            
        }
        n = 0;
        while(n<12){
            if(revs[n][1] == 0){
                t.addEntryInfo(n, new Triple<>(revs[n][0], nNegDist[n] , 0.0f));
            } else {
                t.addEntryInfo(n, new Triple<>(revs[n][0], nNegDist[n] , (float)(cl[n]/revs[n][1])));
            }
            
            n++;
        }
        return t;
    }
/**
     * Business stats per month.
     * @param business_id - String
     * @return Table
     */
    public Table query4(String business_id){ //needs business doesnt exist exception
        Table t = new Table();
        t.setQueryN(4);

        int[][] revs = new int[12][2]; // numEvaluations, nUsersTotais 
        float[] cl = new float[12]; // classificacao
        int[] nUserDist = new int[12]; //nUsers distintos
        int n = 0;
        for(int i = 0; i<12; i++){
            cl[i] = 0;
            nUserDist[i] = 0;
            for(int j = 0; j<2; j++){
                revs[i][j] = 0;
            }
        }
        Map<Integer, List<String>> bus = new TreeMap<>();
        while(n<12){
            bus.put(n, new ArrayList<>());
            n++;
        }

        for (Entry<String,Review> entry : this.reviews.getR_catalog().entrySet()) {
            if(entry.getValue().getBusiness_id().equals(business_id)){
                revs[entry.getValue().getMonthR()-1][0]++;
                revs[entry.getValue().getMonthR()-1][1]++;
                cl[entry.getValue().getMonthR()-1]+=entry.getValue().getStars();
                
                if(!(bus.get(entry.getValue().getMonthR()-1).contains(entry.getValue().get_user_id()))
                    && bus.get(entry.getValue().getMonthR()-1)!=null){
                    bus.get(entry.getValue().getMonthR()-1).add(entry.getValue().get_user_id());
                    nUserDist[entry.getValue().getMonthR()-1]++;
                }
            }            
        }
        n = 0;
        while(n<12){
            if(revs[n][1] == 0){
                t.addEntryInfo(n, new Triple<>(revs[n][0], nUserDist[n] , 0.0f));
            } else {
                t.addEntryInfo(n, new Triple<>(revs[n][0], nUserDist[n] , cl[n]/revs[n][1]));
            }
            n++;
        }
        return t;
    }
 /**
     * Businesses reviewed by User (Ordered).
     * @param user_id - String
     * @return Table
     */
    public Table query5(String user_id){ //needs user doesnt exist exception
        Table t = new Table();
        t.setQueryN(5);
        List<String> list = this.reviews.getReviewsByUser(user_id);
        List<String> listB = list.stream().sorted().collect(Collectors.toList());
        if(listB.size() == 0)
            return new Table();
        for(String id : listB){
            t.addEntryInfo4(this.businesses.get_b_catalog().get(id).get_business_name(), Collections.frequency(listB, id));
        }
       
        t.setCounter1(t.sizeInfo4());
        t.sortInfo4();
        
        return t;
    }
/**
     * Top x businesses per year.
     * @param x - Int
     * @return Table
     */
    public Table query6(int x){
	    Table t = new Table();
        t.setQueryN(6);

        Collection<Review> rev = this.reviews.getR_catalog().values();
        HashMap<Integer,HashMap<String,HashSet<String>>> end = new HashMap<>();
        for(Review e : rev){
            HashSet<String> strings = new HashSet<>();
            HashMap<String,HashSet<String>> usrs_per_rev = new HashMap<>();
            int year = e.getYearR();
            if(end.get(year) == null){
                strings.add(e.get_user_id());
                usrs_per_rev.put(e.getBusiness_id(), strings);
                end.put(year, usrs_per_rev);
            }
            else
            if(end.get(year).get(e.getBusiness_id()) == null){
                strings.add(e.get_user_id());
                end.get(year).put(e.getBusiness_id(),strings);
            }
            else
            end.get(year).get(e.getBusiness_id()).add(e.get_user_id());
        }
        TreeMap<Integer,HashMap<String,Integer>> smt = new TreeMap<>();
        int n = 0;


        
        for(Entry<Integer,HashMap<String,Integer>> yeet : smt.entrySet()){
            HashMap<String,Integer> reverseSortedMap = new LinkedHashMap<>();
            yeet.getValue().entrySet()
            .stream().sorted(Map.Entry.comparingByValue(Comparator.reverseOrder())) 
            .forEachOrdered(a -> reverseSortedMap.put(a.getKey(), a.getValue()));
                              smt.put(yeet.getKey(), reverseSortedMap);
        }
        
        for(Entry<Integer,HashMap<String,HashSet<String>>> year_h : end.entrySet()){
            HashMap<String,HashSet<String>> bus = year_h.getValue();
            HashMap<String,Integer> ert = new HashMap<>();
            n = 0;
            for(Entry<String,HashSet<String>> business : bus.entrySet()){
                if (n < x){
                    ert.put(business.getKey(), business.getValue().size());
                    n++;
                }
                else break;
            }
            smt.put(year_h.getKey(), ert);
        }
        t.setInfo2(smt);
	    return t;
    }
      /**
     * Top three businesses per city.
     * @return Table
     */
    public Table query7(){
	    Table t = new Table();
        t.setQueryN(7);
        Map<String, DataSet> listBC = new TreeMap<String, DataSet>();
        for (Entry<String,Business> entry : this.businesses.get_b_catalog().entrySet()){
            
            if(listBC.containsKey(entry.getValue().get_business_city())){
                int revs = entry.getValue().get_n_reviews();
                String bus = entry.getValue().get_business_name(); 
                listBC.get(entry.getValue().get_business_city()).addB(revs, bus);
            
            } else {
                listBC.put(entry.getValue().get_business_city(), new DataSet());
            }
        }
        for(Entry<String, DataSet> entry : listBC.entrySet()){
            entry.getValue().ordDataSet();
        }

        for(Entry<String,DataSet> entry : listBC.entrySet()){
            HashMap<String, Integer> myNewMap = entry.getValue().getBusSet().entrySet().stream()
                                                     .limit(3)
                                                     .collect(HashMap::new, (m, e) -> m.put(e.getKey(), e.getValue()), Map::putAll);
            t.putEntryInfo3(entry.getKey(), myNewMap);
        }
            
	    return t;
    }
/**
     * X Users with the most unique businesses reviewed.
     * @param x - Int
     * @return Table
     */
    public Table query8(int x){
	    Table t = new Table();
        t.setQueryN(8);
        Map<String, DataSet> usersData = new TreeMap<>();
        Map<String, Integer> info4 = new HashMap<>();
        for (Entry<String,Review> entry : this.reviews.getR_catalog().entrySet()){
            
            if(usersData.containsKey(entry.getValue().get_user_id())){
                int value = usersData.get(entry.getValue().get_user_id()).addRev(entry.getValue().getBusiness_id());
                if(info4.keySet().contains(entry.getValue().get_user_id())){
                    info4.replace(entry.getValue().get_user_id(),value);
                }
            } else {
                usersData.put(entry.getValue().get_user_id(), new DataSet());
                info4.put(entry.getValue().get_user_id(),0);
            }

        }
        List<Entry<String, Integer>> info = new ArrayList<>(info4.entrySet());
        info.sort(Entry.comparingByValue(Comparator.reverseOrder()));
        int num = x;
        for(Entry<String,Integer> entry : info){
                t.addEntryInfo4(entry.getKey(), entry.getValue());
                num--;
                if(num==0) break;
        }
        
	    return t;
    }
     /**
     * Top X users with most reviews in business.
     * @param business_id - String
     * @param x - Int
     * @return Table
     */
    public Table query9(String business_id, int x){ //needs business doesnt exist exception
	    int i = 0;
	    Table o = new Table();
        o.setQueryN(9);

        Collection<Review> review = this.reviews.getR_catalog().values();
        boolean exists = false;
        List<Triple<String,Integer,Float>> triples = new ArrayList<>();
        for(Review rev : review){
            if(rev.getBusiness_id().equals(business_id)){
                Triple<String,Integer,Float> yes = new Triple<String,Integer,Float>(rev.get_user_id(), 1, rev.getStars());
                if(triples.size()==0){
                    triples.add(yes);
                }
                else{
                    for(Triple<String,Integer,Float> triple : triples){
                        if(triple.getFirst().equals(rev.get_user_id())){
                            int no = triple.getSecond()+1;
                            float si = (triple.getThird()+rev.getStars())/2;
                            triple.setSecond(no);
                            triple.setThird(si);
                            exists = true;
                        }
                        else
                            exists = false;

                    }
                    if(!exists)
                    triples.add(yes);
                }
            }
        }

        triples.sort(Comparator.comparing(te -> te.getThird()));
        List<Triple<String,Integer,Float>> sortedTriples = triples.stream().sorted(Comparator.comparing(Triple::getThird)).collect(Collectors.toList());
        Collections.reverse(sortedTriples);


        HashMap<String,Float> de = new HashMap<>();
	    for(Triple<String,Integer,Float> teos : sortedTriples){
            if(i<x){
                de.put(teos.getFirst(),teos.getThird());
                i++;
            }
        }
	    o.setInfo5(de);
	    return o;
    }
    /**
     * Average stars per business per city per state.
     * @return Table
     */
    public Table query10(){
	    Table t = new Table();
        t.setQueryN(10);
        for(Entry<String,Business> entry : this.businesses.get_b_catalog().entrySet()){
            String busCity = entry.getValue().get_business_city();
            String busState = entry.getValue().get_business_state();
            float medClass = entry.getValue().get_m_stars();
            String busName = entry.getValue().get_business_name();
            if(t.containsCityInfo6(busState)){
                t.getValueInfo6(busState).addState(busCity, busName, medClass);
            } else {
                TableStateBus table = new TableStateBus();
                table.addState(busCity, busName, medClass);
                t.addEntryInfo6(busState,table);
            }
             
        }
	    return t;
    }

}
