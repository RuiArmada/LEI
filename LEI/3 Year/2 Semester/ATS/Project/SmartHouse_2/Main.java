import java.util.*;
import java.io.*;
import java.util.Scanner;
import java.util.function.Consumer;
import java.time.LocalDate;

/**
 * Classe que implementa a função Main 
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class Main 
{
    public static void main(String[] args) throws IOException
    {
        // Armazenamento em memória dos dados do ficheiro
        int i = 0;
        String line;
        String homeSupplier = null, homeOwner = null, homeNIF = null, homeRoom = null;
        BufferedReader in;
        Suppliers suppliers = new Suppliers();

        in = new BufferedReader(new FileReader("./Logs.txt"));
        line = in.readLine();
        
        while(line != null)
        {
            String delim1[] = line.split(":");
            String keyword = delim1[0];
            String other = delim1[1];
            
            if(keyword.equals("Fornecedor"))
            {
                suppliers.addEnergySupplier(delim1[1]);
            }
            else if(keyword.equals("Casa"))
            {
                String delim2[] = other.split(",");
                homeOwner = delim2[0];
                homeNIF = delim2[1];
                int nif = Integer.parseInt(homeNIF);
                homeSupplier = delim2[2];
                //System.out.println(homeSupplier);
                
                suppliers.addCasaInteligente(homeOwner, nif, homeSupplier);
            }
            else if(keyword.equals("Divisao"))
            {
                int nif = Integer.parseInt(homeNIF);
                homeRoom = other;
                suppliers.getSupplier(homeSupplier).getHouse(nif).addRoom(other);
            }
            else if(keyword.equals("SmartBulb"))
            {
                String delim2[] = other.split(",");
                String toneBulb = delim2[0];
                int tone;
                if(toneBulb.equals("Warm")) tone = 2;
                else if(toneBulb.equals("Neutral")) tone = 1;
                else tone = 0;
                String dimensionBulb = delim2[1];
                double dimension = Double.parseDouble(dimensionBulb);
                String consumptionBulb = delim2[2];
                double consumption = Double.parseDouble(consumptionBulb);
                int nif = Integer.parseInt(homeNIF);
                SmartDevice devBulb = new SmartBulb(i, tone, dimension, consumption);
                suppliers.addSmartDevice(homeSupplier, nif, i, homeRoom, devBulb);
                i++;
            }
            else if(keyword.equals("SmartCamera"))
            {
                String delim2[] = other.split(",", 3);
                String resolutionCamera = delim2[0];
                resolutionCamera = resolutionCamera.substring(1);
                String delim3[] = resolutionCamera.split("x");
                String xCamera = delim3[0];
                int x = Integer.parseInt(xCamera);
                String yCamera = delim3[1];
                yCamera = yCamera.substring(0,yCamera.length()-1);
                int y = Integer.parseInt(yCamera);
                String sizeCamera = delim2[1];
                double size = Double.parseDouble(sizeCamera);
                String consumptionCamera = delim2[2]; 
                int nif = Integer.parseInt(homeNIF); 
                double consumption = Double.parseDouble(consumptionCamera);              
                SmartDevice devCamera = new SmartCamera(i, x, y, size, consumption);
                suppliers.addSmartDevice(homeSupplier, nif, i, homeRoom, devCamera);
                i++;
            }
            else if(keyword.equals("SmartSpeaker"))
            {
                String delim2[] = other.split(",", 4);
                String volumeSpeaker = delim2[0];
                int volume = Integer.parseInt(volumeSpeaker);
                String channel = delim2[1];
                String brand = delim2[2];
                String consumptionSpeaker = delim2[3];
                double consumption = Double.parseDouble(consumptionSpeaker);
                int nif = Integer.parseInt(homeNIF);
                SmartDevice devSpeaker = new SmartSpeaker(i, channel, brand, volume, consumption);
                suppliers.addSmartDevice(homeSupplier, nif, i, homeRoom, devSpeaker);
                i++;
            }
            
            line = in.readLine();
        }
        
        in.close();

        //Inserir dados através do Menu
        LocalDate date = LocalDate.now();
        Scanner scanner = new Scanner(System.in);
        List<Consumer<Suppliers>> consumers = new ArrayList<Consumer<Suppliers>>();
        Menu x = new Menu();

        while(true)
        {
            int opcao = -1;
            while(opcao < 0 || opcao > 11) 
            {
                opcao = x.MenuInicial(i);
            }

            switch (opcao) 
            {
                case 1: 
                    x.MenuFornecedor(suppliers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 2:
                    x.MenuCasa(suppliers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 3:
                    i = x.MenuDispositivo(suppliers, i);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 4:
                    System.out.print("Introduza os dados a seguir pedidos.\n\n");
                    System.out.print("Dias que passaram: ");
                    int dias = scanner.nextInt();
                    scanner.nextLine();
                    System.out.print("\n");
                
                    LocalDate res = date.plusDays(dias);

                    x.MenuTempo(suppliers,date,res,dias);
                    System.out.print("\n");
                    date = res;

                    if(consumers.size() > 0)
                    {
                        System.out.println("==== Operação em espera executadas: ====");

                        for(Consumer<Suppliers> consumer : consumers)
                        {
                            consumer.accept(suppliers);
                        }

                        Menu.pressEnter();
                    } else {System.out.println("==== Não existiam operações em espera. ====\n");Menu.pressEnter();}

                    consumers = new ArrayList<Consumer<Suppliers>>();
                    break;
                case 5:
                    x.MenuMudarFornecedor(suppliers,consumers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 6:
                    x.MenuOnOff(suppliers,consumers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 7:
                    x.MenuPreco(suppliers,consumers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 8:
                    x.MenuTaxa(suppliers,consumers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 9:
                    x.MenuVolumeFaturacao(suppliers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 10:
                    x.MenuFaturas(suppliers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 11:
                    x.MenuVisualizar(suppliers);
                    System.out.print("\n");
                    Menu.pressEnter();
                    break;
                case 0:
                    scanner.close();
                    System.exit(0);
                    break;
            }
        }
    }
}
