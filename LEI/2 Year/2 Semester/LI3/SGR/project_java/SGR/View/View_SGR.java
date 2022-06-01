package View;

import java.util.Map.Entry;
import java.util.HashMap;
import java.util.Map;

import Util.Triple;
import Model.Model_SGR;
import Model.Table;
import Model.TableStateBus;

public class View_SGR{

	/**
	 * Method that prints the the app Initialization Menu
	 */
	public void show_pre_menu() {
		System.out.println("╔════════════════════════════════════════════╗");
		System.out.println("║############################################║");
		System.out.println("║################ Initialize ################║");
		System.out.println("║############################################║");
		System.out.println("║                                            ║");
		System.out.println("║              1 - Load Database             ║");
		System.out.println("║                                            ║");
		System.out.println("║           2 - Interactive Queries          ║");
		System.out.println("║                                            ║");
		System.out.println("║                  3 - Stats                 ║");
		System.out.println("║                                            ║");
		System.out.println("║               4 - Benchmarks               ║");
		System.out.println("║                                            ║");
		System.out.println("║               5 - Load Status              ║");
		System.out.println("║                                            ║");
		System.out.println("║                  0 - Exit                  ║");
		System.out.println("║                                            ║");
		System.out.println("║############################################║");
		System.out.println("║############################################║");
		System.out.println("║############################################║");
		System.out.println("╚════════════════════════════════════════════╝\n");
		System.out.print("Insert Option: ");
	}
	
	/**
	 * Method that prints the app Main Menu
	 */
	public void show_menu() {
		System.out.println("╔══════════════════════════════════════════════╗");
		System.out.println("║##############################################║");
		System.out.println("║#################### Menu ####################║");
		System.out.println("║##############################################║");
		System.out.println("║                                              ║");
		System.out.println("║                 1 - Query 01                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 2 - Query 02                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 3 - Query 03                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 4 - Query 04                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 5 - Query 05                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 6 - Query 06                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 7 - Query 07                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 8 - Query 08                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 9 - Query 09                 ║");
		System.out.println("║                                              ║");
		System.out.println("║                 0 - Query 10                 ║");
		System.out.println("║                                              ║");
		System.out.println("║ h - Help                                     ║");
		System.out.println("║                                              ║");
		System.out.println("║ s - Save                                     ║");
		System.out.println("║                                              ║");
		System.out.println("║ b - Back                                     ║");
		System.out.println("║                                              ║");
		System.out.println("║##############################################║");
		System.out.println("║##############################################║");
		System.out.println("║##############################################║");
		System.out.println("╚══════════════════════════════════════════════╝\n");
		System.out.print("Insert Option: ");

	}
	/**
	 * Method that prints the Stats Menu
	 * @param model - Model_SGR
	 */
	public void print_stats(Model_SGR model) {
		System.out.println("╔═══════════════════════════════════════════════╗");
		System.out.println("║###############################################║");
		System.out.println("║#################### Stats ####################║");
		System.out.println("║###############################################║");
		System.out.println("╚═══════════════════════════════════════════════╝\n");
		System.out.println("Total Cores:" + Runtime.getRuntime().availableProcessors());
		System.out.println("Operating System:" + System.getProperty("os.name") + "\n");
		printTriple(model.getStats().getFiles());
		System.out.println("\t--> Wrong Reviews: " + model.getStats().getWrong_reviews());
		System.out.println("\t--> Number of Reviews: " + model.getStats().getNumber_reviews());
		System.out.println("\t--> Number of Business: " + model.getStats().getNumber_businesses());
		System.out.println("\t--> Number of Users: " + model.getStats().getNumber_users());
		System.out.println("\t--> Number of Reviewed Businesses: " + model.getStats().getNumber_reviewed_businesses());
		System.out.println("\t--> Number of Non Reviewed Businesses: " + model.getStats().getNumber_non_reviewed_businesses());
		System.out.println("\t--> Number of Users with Reviews: "  + model.getStats().getNumber_users_with_reviews());
		System.out.println("\t--> Number of Users with no Reviews: " + model.getStats().getNumber_users_with_no_reviews());
		System.out.println("\t--> Number of Null Reviews: " + model.getStats().getNull_reviews());
		System.out.println("\t--> Average Stars Globaly: " + model.getStats().getGlobal_average());
		System.out.println("\t--> Number of reviews per Month: ");
		printMapii(model.getStats().getReviews_per_month());
		System.out.println("\t--> Number of different users reviewing per Month: ");
		printMapii(model.getStats().getUsers_per_month());
		System.out.println("\t--> Average Stars per Month: ");
		printMapif(model.getStats().getAverage_per_month());
	} 	
	/**
	 * Method that asks if the User wants to go to the previous menu
	 */
	public void want_back() {
		System.out.println("Press \"b\" if you want to return to the previous screen...");
	} 
	/**
	 * Method that prints a Map of Integers
	 * @param mapii - Map
	 */
	public void printMapii(Map<Integer,Integer> mapii){
		mapii.entrySet().forEach(e->{System.out.println("\t\t--> " + (e.getKey()+1) + "-> "+ e.getValue());});
	}
	/**
	 * Method that prints a Map of Integers and Floats
	 * @param mapif - Map
	 */
	public void printMapif(Map<Integer,Float> mapif){
		mapif.entrySet().forEach(e->{System.out.println("\t\t--> " + (e.getKey()+1) + "-> "+ e.getValue());});
	}
	/**
	 * Method that asks the User if he wants to run Benchmarks
	 */
	public void ask_benchmarks() {
		System.out.println("Do you wish to run benchmarks?");
		System.out.println("Y - Yes  |  N - No");

	}
	/**
	 * Help Menu method
	 * @param func - String
	 */
	public void show_help(String func){
		switch(func) {

			case "query1":
                helpQuery1();
                break;
            
            case "query2":
                helpQuery2();
                break;
            
            case "query3":
                helpQuery3();
                break;
            
            case "query4":
                helpQuery4();
                break;
            
            case "query5":
                helpQuery5();
                break;
            
            case "query6":
                helpQuery6();
                break;
            
            case "query7":
                helpQuery7();
                break;
        
            case "query8":
                helpQuery8();
                break;

            case "query9":
                helpQuery9();
                break;

            case "query10":
                helpQuery10();
                break;

			default:
        		System.out.println("Wrong Input...");
                break;

		}
	} 
	/**
	 * Method that prints the Query number and arguments
	 * @param function - String
	 * @param cmd - String
	 * @param args - String
	 */
	private void helpArgs(String function, String cmd, String args){
        System.out.println(function + ": "+ cmd + args);
    }
	/**
	 * Query 1 help method
	 */
	private void helpQuery1() {
		helpArgs("Query1", "1" , "");
		System.out.println("\tOrdered list of never reviewed businesses and how many there are in total.");
	}
	/**
	 * Query 2 help method
	 */
	private void helpQuery2() {
		helpArgs("Query2", "2" , "month   year");
		System.out.println("\tTotal number of reviews by different users in a specific month and year.");
	}
	/**
	 * Query 3 help method
	 */
	private void helpQuery3() {
		helpArgs("Query3", "3" , "user_ID");
		System.out.println("\tUser stats per month.");
	}
	/**
	 * Query 4 help method
	 */
	private void helpQuery4() {
		helpArgs("Query4", "4" , "business_ID");
		System.out.println("\tBusiness stats per month.");
	}
	/**
	 * Query 5 help method
	 */
	private void helpQuery5() {
		helpArgs("Query5", "5" , "user_ID");
		System.out.println("\tBusinesses reviewed by User (Ordered).");
	}
	/**
	 * Query 6 help method
	 */
	private void helpQuery6() {
		helpArgs("Query6", "6" , "int");
		System.out.println("\tTop x businesses per year.");
	}
	/**
	 * Query 7 help method
	 */
	private void helpQuery7() {
		helpArgs("Query7", "7" , "");
		System.out.println("\tTop three businesses per city.");
	}
	/**
	 * Query 8 help method
	 */
	private void helpQuery8() {
		helpArgs("Query8", "8" , "int");
		System.out.println("\tX Users with the most unique businesses reviewed.");
	}
	/**
	 * Query 9 help method
	 */
	private void helpQuery9() {
		helpArgs("Query9", "9" , "business_ID   int");
		System.out.println("\tTop X users with most reviews in business.");
	}
	/**
	 * Query 10 help method
	 */
	private void helpQuery10() {
		helpArgs("Query10", "10" , "");
		System.out.println("\tAverage stars per business per city per state.");
	}
	/**
	 * Method that asks the User if he wants to Load the Frinds portion of the Files
	 */
	public void ask_friends() {
		System.out.println("Do you intend to load the User FRIENDS ? (High Ram Usage)");
		System.out.println("Y - Yes  |  N - No");
	}
	/**
	 * Method that asks the User if he wants to Load the Default Files
	 */
	public void ask_defaults() {
		System.out.println("Do you intend to load the DEFAULT files ?");
		System.out.println("Y - Yes  |  N - No");
	}
	/**
	 * Method that asks the User File path
	 */
	public void ask_user() {
		System.out.println("╔═══════════════════════════════════════════╗");
		System.out.println("║###########################################║");
		System.out.println("║###########################################║");
		System.out.println("║    Please Insert the Path to USER file    ║");
		System.out.println("║###########################################║");
		System.out.println("║###########################################║");
		System.out.println("╚═══════════════════════════════════════════╝\n");
		System.out.print("-> ");
	} 
	/**
	 * Method that asks the Save File name
	 */
	public void ask_save() {
		System.out.println("╔═══════════════════════════════════════════╗");
		System.out.println("║###########################################║");
		System.out.println("║###########################################║");
		System.out.println("║ Please Insert the File to save the status ║");
		System.out.println("║###########################################║");
		System.out.println("║###########################################║");
		System.out.println("╚═══════════════════════════════════════════╝\n");
		System.out.print("-> ");
	}
	/**
	 * Method that asks the Business File path
	 */
	public void ask_business() {
		System.out.println("╔═══════════════════════════════════════════════╗");
		System.out.println("║###############################################║");
		System.out.println("║###############################################║");
		System.out.println("║    Please Insert the Path to BUSINESS file    ║");
		System.out.println("║###############################################║");
		System.out.println("║###############################################║");
		System.out.println("╚═══════════════════════════════════════════════╝\n");
		System.out.print("-> ");
	}
	/**
	 * Method that asks the Review File path
	 */
	public void ask_review() {
		System.out.println("╔═════════════════════════════════════════════╗");
		System.out.println("║#############################################║");
		System.out.println("║#############################################║");
		System.out.println("║    Please Insert the Path to REVIEW file    ║");
		System.out.println("║#############################################║");
		System.out.println("║#############################################║");
		System.out.println("╚═════════════════════════════════════════════╝\n");
		System.out.print("-> ");
	}
	/**
	 * Method that warns that the Database is not Loaded
	 */
	public void not_loaded() {
		System.out.println("╔═══════════════════════════════════════════╗");
		System.out.println("║###########################################║");
		System.out.println("║###########################################║");
		System.out.println("║      WARNING: Database is not loaded      ║");
		System.out.println("║###########################################║");
		System.out.println("║###########################################║");
		System.out.println("╚═══════════════════════════════════════════╝\n");
	}
	/**
	 * Methond that asks the User the number of the desired Query
	 */
	public void ask_option() {
		System.out.println("What is the query that you need help with?");
		System.out.print("Insert number of desired query: ");
	}
	/**
	 * Method that beautifies a String
	 * @param s - String
	 */
	public void show(String s) {
		System.out.println("------>" + s + "<------");
	} 
	/**
	 * Method that asks the User for a Business ID
	 */
	public void ask_business_id() {
		System.out.print("Insert a VALID Business ID: ");
	}
	/**
	 * Method that asks the User for a Year
	 */ 
	public void ask_year() {
		System.out.print("Insert a VALID Year (YYYY): ");
	}
	/**
	 * Method that asks the User for a Month
	 */
	public void ask_month() {
		System.out.print("Insert a VALID Month (MM): ");
	}
	/**
	 * Method that asks the User for a User ID
	 */
	public void ask_user_id() {
		System.out.print("Insert a VALID USER ID: ");
	} 
	/**
	 * Method that asks for the number of wanted Businesses for Query 6 
	 */
	public void ask_int_query_6() {
		System.out.print("Insert number of wanted Businesses: ");
	} 
	/**
	 * Method that asks for the number of wanted Users for Query 8
	 */
	public void ask_int_query_8() {
		System.out.print("Insert number of wanted Users: ");
	} 
	/**
	 * Method that asks for the number of wanted Users for Query 9 
	 */
	public void ask_int_query_9() {
		System.out.print("Insert number of wanted Users: ");
	}
	/**
	 * Method that prints the output of query 2
	 * @param year - Int
	 * @param month - Int
	 * @param counter1 - Int
	 * @param counter2 - Int
	 */
	public void output_query_2(int year, int month, int counter1, int counter2) {
		System.out.println("In " + month + " of " + year);
		System.out.print("\tThere were " + counter1 + " reviews made by " + counter2 + " distinct users.");
	}
	/**
	 * Begin Benchmarking Warning menu method
	 */
	public void benchmark_begin() {
			System.out.println("╔══════════════════════════════╗");
			System.out.println("║##############################║");
			System.out.println("║##############################║");
			System.out.println("║    Beginning Benchmarking    ║");
			System.out.println("║##############################║");
			System.out.println("║##############################║");
			System.out.println("╚══════════════════════════════╝\n");
	}
	/**
	 * End Benchmarking Warning menu method
	 */
	public void benchmark_end() {
		System.out.println("╔══════════════════════════════╗");
		System.out.println("║##############################║");
		System.out.println("║##############################║");
		System.out.println("║      Ending Benchmarking     ║");
		System.out.println("║##############################║");
		System.out.println("║##############################║");
		System.out.println("╚══════════════════════════════╝\n");
	}
	/**
	 * Method that prints the resulting table of each Query
	 * @param t - Table
	 */
	public void printQueryResult(Table t){
		switch(t.getQueryN()) {

			case 1:
				for(String st : t.getCodNames()){
					System.out.println(st);
				}
				System.out.println("Business total number: "+t.getCounter1()+"\n");
				break;

			case 2:
				System.out.println("Reviews total number: "+t.getCounter1()+"\n");
				System.out.println("Number of different users: "+t.getCounter2()+"\n");
				break;

			case 3:
				for(Entry<Integer, Triple<Integer,Integer,Float>> entry : t.getInfo().entrySet()){
					System.out.println("Month: "+(entry.getKey())+"\n");
					System.out.println("\tReviews number: "+entry.getValue().getFirst()+"\n");
					System.out.println("\tNumber of reviewed businesses: "+entry.getValue().getSecond()+"\n");
					System.out.println("\tAverage classification: "+entry.getValue().getThird()+"\n");
				}
				break;
			
			case 4:
				for(Entry<Integer, Triple<Integer,Integer,Float>> entry : t.getInfo().entrySet()){
					System.out.println("Month : "+(entry.getKey())+"\n");
					System.out.println("\tNumber of reviews: "+entry.getValue().getFirst()+"\n");
					System.out.println("\tNumber of users: "+entry.getValue().getSecond()+"\n");
					System.out.println("\tAverage classification: "+entry.getValue().getThird()+"\n");
				}	
				break;
			
			case 5:
				System.out.println("Business name : Reviews' number\n");
				for(Entry<String, Integer> entry : t.getInfo4().entrySet()){
					System.out.println(entry.getKey()+" : "+entry.getValue()+"\n");
				}
				System.out.println("Number of reviewed businesses: "+t.getCounter1()+"\n");
				break;	

			case 6:
				for(Entry<Integer, HashMap<String,Integer>> entry : t.getInfo2().entrySet()){
					System.out.println("\nYear : "+entry.getKey()+"\n");
					String table = entry.getValue().toString();
					System.out.println(table);
				}	
				break;	
			
			case 7:
				for(Entry<String, Map<String,Integer>> entry : t.getInfo3().entrySet()){
					System.out.println("City: "+entry.getKey()+"\n");
					int n = 1;
					for(Entry<String,Integer> entry2 : entry.getValue().entrySet()){
						System.out.println("\t"+n+"o: "+entry2.getKey()+" - "+entry2.getValue()+"\n");
						n++;
					}
				}
					break;
				
			case 8:
					for(Entry<String, Integer> entry : t.getInfo4().entrySet()){
						System.out.println(entry.getKey()+" : "+entry.getValue()+"\n");
					}
					break;
				
			case 9:
					for(Entry<String, Float> entry : t.getInfo5().entrySet()){
						System.out.println(entry.getKey()+" : "+entry.getValue()+"\n");
					}
					break;
				
			case 10:
					for(Entry<String,TableStateBus> entry : t.getInfo6().entrySet()){
						System.out.println("State :"+entry.getKey()+"\n");
						String table = entry.getValue().toString();
						System.out.println(table+"\n");
					}
					break;	
			}
		}
	/**
	 * Method that prints a Triple of strings
	 * @param a - Triple
	 */
	public void printTriple(Triple<String,String,String> a){
		System.out.println("\t--> Files:");
		System.out.println("\t\t--> " + a.getFirst());
		System.out.println("\t\t--> " + a.getSecond());
		System.out.println("\t\t--> " + a.getThird());

	}
}
