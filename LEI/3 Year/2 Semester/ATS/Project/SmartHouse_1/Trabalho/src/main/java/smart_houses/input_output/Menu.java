package smart_houses.input_output;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Menu {
    // opcao escolhida no menu
    private int opcao;
    //lista com as opcoes do menu
    private List<String> opcoes;
    // scanner para receber o input
    private Scanner scan;

    /**
     * Contruto parametrizado
     * @param opcoes lista de opcoes do menu
     */
    public Menu(List<String> opcoes){
        this.opcoes = new ArrayList<>(opcoes);
        this.opcao = -1;
        this.scan = new Scanner(System.in);
    }

    /**
     * getter para a opcao escolhida
     * @return valor da opcao escolhida
     */
    public int getOpcao() {
        return opcao;
    }

    /**
     * Metodo que executa o menu
     */
    public void run(){
        do{
            this.showMenu();
            this.opcao = this.scan.nextInt();
            this.scan.nextLine();
        } while(this.opcao == -1);
    }

    /**
     * Metodo que da print de todas as opcoes disponiveis
     */
    private void showMenu(){
        for(String opcao : this.opcoes){
            System.out.println(opcao);
        }
    }

}
