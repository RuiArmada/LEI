package Controler;

import java.io.IOException;

import Util.ClearScreen;
import Util.Crono;
import Util.Input;
import Model.Model_SGR;
import View.View_SGR;
import Model.Table;

public class Controller{
	private Model_SGR model;
	private View_SGR view;
	
	/**
	 * Empty constructor Method for Controller class
	 */
	public Controller(){
		this.model = new Model_SGR();
		this.view = new View_SGR();
	}
	/**
	 * Allows the change of said variable
	 * @param m - Model_SGR
	 */
	void set_model(Model_SGR m){
		this.model = m;
	}
	/**
	 * Allows the change of said variable
	 * @param v - View_SGR
	 */
	void set_view(View_SGR v){
		this.view = v;
	}

	/**
	 * Method that asks if user wasnts to load Friends and reads the input
	 * @return - boolean
	 */
	public boolean ask_friends(){
		Input input = new Input();
		String in = "";
		ClearScreen.clear_screen();
		view.ask_friends();
		in = input.lerString();
		in = in.toLowerCase();
		return(!in.contains("n"));
	}

	/**
	 * Method that asks for files and reads the input from the User
	 * @return - boolean
	 */
	public boolean ask_files(){
		Input input = new Input();
		String in = "";
		//ClearScreen.clear_screen();
		view.ask_defaults();
		in = input.lerString();
		in = in.toLowerCase();
		return(in.contains("n"));
	}

	/**
	 * Method that deals with the Stats portion of the Application
	 */
	public void stats(){
		Input input = new Input();

		String in = "";
		do{
			ClearScreen.clear_screen();
			model.get_per_month_info();
			model.get_stats_reviewed_businesses_in_cat();
			model.get_stats_users_in_cat();
			view.print_stats(model);
			view.want_back();
			in = input.lerString();
			in = in.toLowerCase();
		}while(!in.equals("b"));
		ClearScreen.clear_screen();
	}
	/**
	 * Method that deals with que queries and feeds them the input from the User
	 */
	public void queries_loop(){
		Input input = new Input();
		Input input2 = new Input();
		String in = "";
		int x = 0;
		String id = "";
		Table t = new Table();
		String time = "";
		int year;
		int month;
		String hold = "";
		
		do{

			ClearScreen.clear_screen();
			view.show_menu();
			in = input.lerString();
			in = in.toUpperCase();

			switch(in){

				case "1":
					Crono.start();
					t = model.query1();
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "2":
					view.ask_year();
					year = input.lerInt();
					view.ask_month();
					month = input.lerInt();
					ClearScreen.clear_screen();
					Crono.start();
					t = model.query2(year, month);
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "3":
					view.ask_user_id();
					id = input.lerString();
					ClearScreen.clear_screen();
					Crono.start();
					t = model.query3(id);
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;
				
				case "4":
					view.ask_business_id();
					id = input.lerString();
					ClearScreen.clear_screen();
					Crono.start();
					t = model.query4(id);
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "5":
					view.ask_user_id();
					id = input.lerString();
					ClearScreen.clear_screen();
					Crono.start();
					t = model.query5(id);
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "6":
					view.ask_int_query_6();
					x = input.lerInt();
					ClearScreen.clear_screen();
					Crono.start();
					t = model.query6(x);
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "7":
					Crono.start();
					t = model.query7();
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "8":
					view.ask_int_query_8();
					x = input.lerInt();
					ClearScreen.clear_screen();
					Crono.start();
					t = model.query8(x);
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "9":
					view.ask_business_id();
					id = input.lerString();
					view.ask_int_query_9();
					x = input.lerInt();
					ClearScreen.clear_screen();
					Crono.start();
					t = model.query9(id,x);
					time = Crono.get_time_to_stringln();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "0":
					Crono.start();
					t = model.query10();
					time = Crono.get_time_to_string();
					view.printQueryResult(t);
					hold = input.lerString();
					System.out.print(time);
					break;

				case "S":
					Crono.start();
					view.ask_save();
					id = input.lerString();
					try{
						model.save(id);
					}
						catch(IOException e){
							System.out.println("Failed to save the file");
						}
					time = Crono.get_time_to_string();
					System.out.print(time);
					break;

				case "H":
					ClearScreen.clear_screen();
					view.ask_option();
					in = input2.lerString();
					in = in.toUpperCase();
					view.show_help(in);
					break;

				default:
					System.out.print("Wrong Input, please use one of the id's showed in the Menu.\n");

			}

		}while(!in.equals("B"));
		hold.length();
	}
	/**
	 * Method that starts the Controller - Main Controller Method
	 */
	public void start(){
		Input input = new Input();
		String op = "";
		String time = "";
		Model_SGR n = new Model_SGR();
		set_model(n);
		boolean loaded = false;
		ClearScreen.clear_screen();
		
		do{
			view.show_pre_menu();
			op = input.lerString();
			op = op.toUpperCase();

			switch(op){

				case "1" :
					Crono.start();
					ClearScreen.clear_screen();
					Parser parser = new Parser();
					String file1 = "";
					String file2 = "";
					String file3 = "";
					parser.set_friends(ask_friends());
					if(ask_files()){
						ClearScreen.clear_screen();
						view.ask_user();
						file1 = input.lerString();
						view.ask_business();
						file2 = input.lerString();
						view.ask_review();
						file3 = input.lerString();
						parser.open_files(file1, file2, file3);
						this.model.set_files_stats(file1, file2, file3);
					}
					else{
						this.model.set_files_stats();
						parser.open_files();
					}
					System.out.println("Files are being loaded.");
					parser.parse_full(this.model);
					loaded = true;
					time = Crono.get_time_to_stringln();
					System.out.print(time);

					break;
				case "2" :
					ClearScreen.clear_screen();
					if(loaded){
						queries_loop();
					}
					else
						view.not_loaded();
					break;
   				case "3" :
					ClearScreen.clear_screen();
					if(loaded){
						stats();
					}
					else
						view.not_loaded();
					break;

				case "4":
					Crono.start();
					ClearScreen.clear_screen();
					Benchmarks.Query_Testing.Testing();
					time = Crono.get_time_to_stringln();
					System.out.print(time);
					break;
				
				case "5":
					Crono.start();
					ClearScreen.clear_screen();
					view.ask_save();
					op = input.lerString();
					try{set_model(model.read(op));}
					catch(IOException e){
						System.out.println("Error opening file");
					}
					catch(ClassNotFoundException e){
						System.out.println("Error Class not found");
					}
					time = Crono.get_time_to_stringln();
					System.out.print(time);
					loaded = true;
					break;
					
				case "0":
					ClearScreen.clear_screen();
					break;
				
				default:
					ClearScreen.clear_screen();
					view.show("Bad Input");

			}

		}while(!op.equals("0"));
	}

}
