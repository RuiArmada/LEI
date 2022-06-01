package Util;

public class ClearScreen {

    /**
     * Method that Clears the Screen
     */
    public static void clear_screen() {
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }

}
