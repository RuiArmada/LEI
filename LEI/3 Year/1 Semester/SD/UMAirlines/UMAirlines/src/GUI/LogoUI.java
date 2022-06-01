package UMAirlines.src.GUI;

/**
 * Class responsable for the UI of the project.
 */
public class LogoUI {

    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_ORANGE = "\u001B[33m";
    public static final String ANSI_RESET = "\u001B[0m";


    /**
     * Draws the application's logo
     */
    public static void Logo(){
        System.out.print("\n\033[H\033[2J");
        System.out.flush();
        System.out.println(


        ANSI_RED + "$$\\   $$\\ $$\\       $$\\"+ ANSI_ORANGE+"  "+"  $$$$$$\\   $$\\           $$\\ $$\\\n"+
        ANSI_RED + "$$ |  $$ |$$$\\     $$$ |"  + ANSI_ORANGE+"  "+" $$  __$$\\ \\__|          $$ |\\__|\n"+
        ANSI_RED + "$$ |  $$ |$$$$\\   $$$$ |"  + ANSI_ORANGE+"  "+" $$ /  $$ |$$\\  $$$$$$\\  $$ |$$\\ $$$$$$$\\   $$$$$$\\   $$$$$$$\\\n"+
        ANSI_RED + "$$ |  $$ |$$\\$$\\$$  $$ |" + ANSI_ORANGE+"  "+" $$$$$$$$ |$$ |$$  __$$\\ $$ |$$ |$$  __$$\\ $$  __$$\\ $$  _____|\n"+
        ANSI_RED + "$$ |  $$ |$$ \\$$$   $$ |"  + ANSI_ORANGE+"  "+" $$  __$$ |$$ |$$ |  \\__|$$ |$$ |$$ |  $$ |$$$$$$$$ |\\$$$$$$\\\n"+
        ANSI_RED + "$$ |  $$ |$$ |\\$  / $$ |"  + ANSI_ORANGE+"  "+" $$ |  $$ |$$ |$$ |      $$ |$$ |$$ |  $$ |$$   ____| \\____$$\\\n"+
        ANSI_RED + "\\$$$$$$  |$$ | \\_/  $$ |" + ANSI_ORANGE+"  "+" $$ |  $$ |$$ |$$ |      $$ |$$ |$$ |  $$ |\\$$$$$$$\\ $$$$$$$  |\n"+
        ANSI_RED + " \\______/ \\__|      \\__|"+ ANSI_ORANGE+"  "+" \\__|  \\__|\\__|\\__|      \\__|\\__|\\__|  \\__| \\_______|\\_______/\n"+
        ANSI_RESET);
    };
}
