package Controler;
import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;


import Util.Exceptions.Invalid_Review_Exception;
import Model.Model_SGR;

public class Parser{
	private int n_invalid_users;
	private int n_invalid_reviews;
	private int n_invalid_business;
	private File reviews; 
	private File users;
	private File business;
	private boolean friends;

	/**
	 * Empty constructor Method for Parser class
	 */
	public Parser(){
		this.n_invalid_business = 0;
		this.n_invalid_reviews = 0;
		this.n_invalid_users = 0;
		this.reviews = null;
		this.users = null;
		this.business = null;
		this.friends = false;
		

	}
	/**
	 * Constructor Method for Parser class
	 * @param n_invalid_business - Int
	 * @param n_invalid_reviews - Int
	 * @param n_invalid_users - Int
	 * @param reviews - File
	 * @param users - File
	 * @param business - File
	 * @param friends - Boolean
	 */
	public Parser(int n_invalid_business,int n_invalid_reviews, int n_invalid_users, File reviews, File users, File business,boolean friends){
		this.n_invalid_business = n_invalid_business;
		this.n_invalid_reviews = n_invalid_reviews;
		this.n_invalid_users = n_invalid_users;
		this.reviews = reviews;
		this.users = users;
		this.business = business;
		this.friends = friends;
	}

	/**
	 * Allows access to said variable
	 * @return business - File
	 */
	public File getBusiness() {
		return business;
	}
	/**
	 * Allows access to said variable
	 * @return reviews - File
	 */
	public File getReviews() {
		return reviews;
	}
	/**
	 * Allows access to said variable
	 * @return users - File
	 */
	public File getUsers() {
		return users;
	}
	/**
	 * Allows access to said variable
	 * @return n_invalid_business - Int
	 */
	public int getN_invalid_business() {
		return n_invalid_business;
	}
	/**
	 * Allows access to said variable
	 * @return n_invalid_reviews - Int
	 */
	public int getN_invalid_reviews() {
		return n_invalid_reviews;
	}
	/**
	 * Allows access to said variable
	 * @return n_invalid_users - Int
	 */
	public int getN_invalid_users() {
		return n_invalid_users;
	}
	/**
	 * Allows access to said variable
	 * @return friends - Bollean
	 */
	public boolean get_friends(){
		return friends;
	}

	/**
	 * Allows change of said variable
	 * @param n_invalid_business - Int
	 */
	public void setN_invalid_business(int n_invalid_business) {
		this.n_invalid_business = n_invalid_business;
	}
	/**
	 * Allows change of said variable
	 * @param n_invalid_reviews - Int
	 */
	public void setN_invalid_reviews(int n_invalid_reviews) {
		this.n_invalid_reviews = n_invalid_reviews;
	}
	/**
	 * Allows change of said variable
	 * @param n_invalid_users - Int
	 */
	public void setN_invalid_users(int n_invalid_users) {
		this.n_invalid_users = n_invalid_users;
	}
	/**
	 * Allows change of said variable
	 * @param business - File
	 */
	public void setBusiness(File business) {
		this.business = business;
	}
	/**
	 * Allows change of said variable
	 * @param reviews - File
	 */
	public void setReviews(File reviews) {
		this.reviews = reviews;
	}
	/**
	 * Allows change of said variable
	 * @param users - File
	 */
	public void setUsers(File users) {
		this.users = users;
	}
	/**
	 * Allows change of said variable
	 * @param friends - Boolean
	 */
	public void set_friends(boolean friends){
		this.friends = friends;
	}

	//--------------------------------------------------------------------------------------------------------------------------------------

	/**
	 * Method that opens the Files from a predefined path
	 */
	public void open_files(){
		File reviews = new File("../Database/reviews_1M.csv");
		File users = new File("../Database/users_full.csv");
		File business = new File("../Database/business_full.csv");
		setUsers(users);
		setReviews(reviews);
		setBusiness(business);
	}
	/**
	 * Method that opens the Files with the paths given by input
	 * @param p_user - String
	 * @param p_business - String
	 * @param p_reviews - String
	 */
	public void open_files(String p_user, String p_business, String p_reviews){
		File reviews = new File(p_reviews);
		File users = new File(p_user);
		File business = new File(p_business);
		setUsers(users);
		setReviews(reviews);
		setBusiness(business);
	}
	/**
	 * Method that parses the User File
	 * @param model - Model_SGR
	 * @throws FileNotFoundException - Exception
	 */
	public void parse_users(Model_SGR model) throws FileNotFoundException{
		BufferedReader br = null;

		try {br = new BufferedReader(new FileReader(this.users));}
        catch (FileNotFoundException e) {
            System.out.println("Can't find csv file.");
            System.exit(1);
        }

		String st = null;
        while (true) {
            try {
                if (!((st = br.readLine()) != null)) break;
            } catch (IOException | NullPointerException e) {
                System.out.println("CSV file is null.");
            }
	    String[] info = new String[3];
	    String x = st + ";";
	    boolean error = false;
	    int iPos = 0;
	    int iStr = 0; 
	    int iNext = -1;
	    while( (iNext = x.indexOf( ';', iPos )) != -1 && iStr < 7 ){
		    if( iNext == iPos ){
			    error = true;
		    } else {
			    info[iStr++] = x.substring( iPos, iNext );
		    }
		    iPos = iNext + 1;
	    }
	    if (error){
		    this.n_invalid_users++;
	    } else {
		    if (this.friends){
			    String[] friends = info[2].split(","); 
			    Set<String> fSet = new HashSet<>();
			    for (String id : friends) {
				    fSet.add(id);
			    }
			    model.addUser(info[0], info[1], fSet);
		    } else {
			    model.addUser(info[0], info[1]);
		    }
	    }
	}
	}
	/**
	 * Methos that parses the Business File
	 * @param model - Model_SGR
	 * @throws FileNotFoundException - Exception
	 */
	public void parse_businesses(Model_SGR model) throws FileNotFoundException{
		BufferedReader br = null;

		try {br = new BufferedReader(new FileReader(this.business));}
		catch (FileNotFoundException e) {
			System.out.println("Can't find csv file.");
			System.exit(1);
		}

		String st = null;
        	while (true) {
            		try {
                		if (!((st = br.readLine()) != null)) break;
			} 
			catch (IOException | NullPointerException e) {
				System.out.println("CSV file is null.");
			}
			String end = st + ";";
			String[] info = end.split(";");
			if (info.length != 5){
				this.n_invalid_business++;
			} else {
				String[] cat = info[4].split(","); 
				Set<String> cSet = new HashSet<>();
				for (String id : cat) {
					cSet.add(id);
				}
				model.add_business(info[0], info[1],info[2],info[3], cSet);
			}
		}
	}
	/**
	 * Method that parses the Reviews File
	 * @param model - Model_SGR
	 * @throws FileNotFoundException - Exception
	 */
	public void parse_reviews(Model_SGR model) throws FileNotFoundException{    
		BufferedReader br = null;
                try {        
			br = new BufferedReader(new FileReader(this.reviews));        
                }        
                catch (FileNotFoundException e) {        
                        System.out.println("Can't find csv file.");        
                        System.exit(1);        
                }        
		try{
			br.readLine(); // this will read the first line
                
		}catch(IOException e){
			System.out.print("IO ERROR");
			System.exit(1);
		}
		String st = null;        
                while (true) {        
                        try {        
                                if (!((st = br.readLine()) != null)) break;        
                        } catch (IOException | NullPointerException e) {        
                                System.out.println("CSV file is null.");        
                        }        
                        String[] info = st.split(";");        
                        if (info.length != 9){        
                                this.n_invalid_reviews++;        
                        } else {        
                                float stars = Float.parseFloat(info[3]);        
                                int usefull = Integer.parseInt(info[4]);        
                                int funny = Integer.parseInt(info[5]);        
                                int cool = Integer.parseInt(info[6]);        
                                SimpleDateFormat formatter=new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");        
                                try{        
                                        Date date=formatter.parse(info[7]);
										Date now = new Date();
										if(date.getYear() <= now.getYear()){
											model.add_review(info[0], info[1], info[2],stars,usefull,funny,cool,date,info[8]);
										} else {
											this.n_invalid_reviews++;
										}        
                                                
                                }        
                                catch(Invalid_Review_Exception e1){        
					this.n_invalid_reviews++;
                                        e1.toString();        
                                }        
                                catch(ParseException e2){        
                                        System.out.println("Error Parsing String to Date");        
                                }        
                        }        
                }        
        }
	/**
	 * Method that Parses all the files at the same time
	 * @param model - Model_SGR
	 */
	public void parse_full(Model_SGR model){
		try{
			parse_users(model);
			System.out.println("Users LOADED");
			parse_businesses(model);
			System.out.println("Businesses LOADED");
			parse_reviews(model);
			System.out.println("Reviews LOADED");
		}
		catch(FileNotFoundException e){
			System.out.println("Erro: File not found\n");
		}
	}
}
