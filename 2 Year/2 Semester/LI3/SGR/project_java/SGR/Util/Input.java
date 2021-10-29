package Util;


import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import static java.lang.System.in;
import java.util.Scanner;
import java.util.InputMismatchException;

public class Input {
	private Scanner input;
	
	/**
	 * Empthy constructor for Input class 
	 */
	public Input(){
		this.input = new Scanner(in);
	}
	/**
	 * Method that reads a input string
	 * @return txt - String
	 */
	public String lerString() {
		input = new Scanner(in);
		boolean ok = false; 
		String txt = "";
		while(!ok && input.hasNextLine()) {
			try {
				txt = input.nextLine();
				ok = true;
			}
			catch(InputMismatchException e){
				System.out.print("Invalid String.\n");
				System.out.print("New Input: ");
				input.nextLine();
			}
		}
		//input.close();
		return txt;
	}
	/**
	 * Method that reads a input Int
	 * @return i - Int
	 */
	public int lerInt() {
		input = new Scanner(in);
		boolean ok = false; 
		int i = 0; 
		while(!ok) {
			try {
				i = input.nextInt();
				ok = true;
			}
			catch(InputMismatchException e) {
				System.out.print("Invalid Integer.\n");
				System.out.print("New Integer: ");
				input.nextLine();
			}
		}
		//input.close();
		return i;
	}
	/**
	 * Method that reads a input Date
	 * @return date - Date
	 */
	public Date ler_date_time() {
		input = new Scanner(in);
		SimpleDateFormat formatter=new SimpleDateFormat("MM-yyyy");
		boolean out = true;
		String t = "";
		while(out){
			try{
				t = input.nextLine();
				out = false;
			}
			catch(InputMismatchException e){
				System.out.print("Invalid Date.\n");
				System.out.print("New Date (mm-yyyy): ");
				input.nextLine();
			}
		}
		//input.close();
		Date date = new Date();
		try{
			date=formatter.parse(t);
		}catch(ParseException d){
			System.out.println("Error Parsing String to Date");
		}
		return date;
	}
}
