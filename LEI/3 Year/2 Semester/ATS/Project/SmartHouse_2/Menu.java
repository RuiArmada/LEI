import java.util.Scanner;
import java.util.function.Consumer;
import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.time.LocalDate;
import java.util.Locale;

/**
 * Classe que implementa o Menu do Utilizador
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class Menu 
{
    Scanner scanner = new Scanner(System.in).useLocale(Locale.US);

    public int MenuInicial(int id) 
    {
        
        clearWindow();
        StringBuilder sb = new StringBuilder("-----------MENU INICIAL-----------\n\n");
        sb.append("1) Criar fornecedor de energia.\n");
        sb.append("2) Criar casa.\n");
        sb.append("3) Criar dispositivo.\n");
        sb.append("4) Avançar o tempo.\n");
        sb.append("5) Mudar fornecedor de energia.\n");
        sb.append("6) Ligar ou desligar dispositivos.\n");
        sb.append("7) Alterar preço de fornecedor.\n");
        sb.append("8) Alterar impostos dos fornecedores.\n");
        sb.append("9) Saber qual o fornecedor com maior volume de faturação.\n");
        sb.append("10) Visualizar as faturas emitidas por um fornecedor.\n");
        sb.append("11) Visualizar o estado do programa.\n");
        sb.append("0) Sair.\n\n");
        sb.append("Selecione a opção pretendida: ");
        System.out.println(sb.toString());
        int res=scanner.nextInt();
        scanner.nextLine();
        return res;
    }
    
    public static String pressEnter()
    {
        System.out.println("Pressione qualquer tecla para continuar...");
        Scanner scanner1 = new Scanner(System.in).useLocale(Locale.US);
        return scanner1.nextLine();
    }

    public static void clearWindow() 
    {
        for (int i = 0;i<100;i++)
        {
            System.out.println();
        }
    }

//----------------------------------------1--------------------------------------------------------//

/**
 * Método que se ocupa de criar um fornecedor de energia
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 */
public void MenuFornecedor(Suppliers suppliers) 
{ 
    StringBuilder sb = new StringBuilder("-----------MENU FORNECEDOR-----------\n\n");
    sb.append("Introduza os dados a seguir pedidos.\n\n");
    sb.append("Companhia: ");
    
    System.out.println(sb.toString());
    String companhia = scanner.nextLine();

    Erros erro = new Erros();
    erro.erroExisteFornecedor(suppliers, companhia);
    if(erro.getBool()) {
        System.out.println(erro);
        return;
    }


    double preco;
    int l = companhia.length();
        if(l>=10) preco = l*0.1;
        else preco = l*0.2;

    System.out.println("Custo diário calculado automáticamente foi de: "+preco);

    System.out.print("Deseja manter este consumo?[Y/N] ");
    String res = scanner.nextLine();
    System.out.print("\n");

    if(res.equals("N"))
    {
        System.out.print("Custo diário a manter: ");
        Double x = scanner.nextDouble();
        scanner.nextLine();
        System.out.print("\n");

        System.out.println("Impostos estão a 23%.");

        suppliers.addEnergySupplier(companhia,x);
    }
    else
    {
        System.out.println("Impostos estão a 23%.");
        suppliers.addEnergySupplier(companhia,preco);
    }

}

//-------------------------------------------2---------------------------------------------------//

/**
 * Método que se ocupa de criar uma casa
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 */
public void MenuCasa(Suppliers suppliers)
{
    System.out.print("-----------MENU CASA-----------\n\n");
    System.out.print("Introduza os dados a seguir pedidos.\n\n");
    System.out.print("Dono: ");
    String dono = scanner.nextLine();
    System.out.print("\n");

    System.out.print("NIF: ");
    int nif = scanner.nextInt();
    scanner.nextLine();
    System.out.print("\n");

    System.out.println("Dos seguintes fornecedores escolha o que quer associar à casa: ");
    
    for(String str:suppliers.getSuppliers().keySet())
    {
        System.out.println(str);
    }

    System.out.print("Fornecedor: ");
    String fornecedor = scanner.nextLine();
    System.out.print("\n");

    Erros erro = new Erros();
    erro.erroExisteFornecedor(suppliers, fornecedor);
    if(erro.getBool()==false) {
        System.out.println(erro);
        return;
    }

    erro = new Erros();
    erro.erroExisteCasaInteligente(suppliers, fornecedor, nif);
    if(erro.getBool()) {
        System.out.println(erro);
        return;
    }

    suppliers.addCasaInteligente(dono,nif,fornecedor);
}

//-------------------------------------------3-----------------------------------------------------//

/**
 * Método que se ocupa de adicionar um dispositivo
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 * @param id var. que guarda o id do proximo device a ser adicionado
 * @return id var. que guarda o id do proximo device a ser adicionado
 */
public int MenuDispositivo(Suppliers suppliers, int id)
{
    Scanner scanner = new Scanner(System.in).useLocale(Locale.US);
    System.out.print("-----------MENU DISPOSITIVO-----------\n\n");
    System.out.print("Introduza os dados a seguir pedidos.\n\n");

    System.out.println("Dos seguintes fornecedores escolha o que está associado: ");
    for(String str:suppliers.getSuppliers().keySet())
    {
        System.out.println(str);
    }

    System.out.print("Fornecedor: ");
    String fornecedor = scanner.nextLine();
    System.out.print("\n");

    Erros erro = new Erros();
    erro.erroExisteFornecedor(suppliers, fornecedor);
    if(erro.getBool()==false) {
        System.out.println(erro);
        return id;
    }

    System.out.println("Lista de nifs: \n");

    for(Integer str:suppliers.getSupplier(fornecedor).getHouses().keySet())
    {
        System.out.println(str);
    }    

    System.out.print("NIF: ");
    int nif = scanner.nextInt();
    scanner.nextLine();
    System.out.print("\n");

    Erros erro2 = new Erros();
    erro2.erroExisteCasaInteligente(suppliers, fornecedor, nif);
    if(erro2.getBool()==false) {
        System.out.println(erro2);
        return id;
    }

    System.out.println("Das seguintes divisões escolha a que está associada: ");
    for(String str:suppliers.getSupplier(fornecedor).getHouse(nif).getLocations().keySet())
    {
        System.out.println(str);
    }
    System.out.print("Divisão: ");
    String room = scanner.nextLine();
    System.out.print("\n");

    Erros erro3 = new Erros();
    erro3.erroExisteDivisao(suppliers, fornecedor, nif, room);
    if(erro3.getBool()==false) {
        System.out.println(erro3);
        return id;
    }

    System.out.println("O ID é: "+(id+1));
    System.out.print("O preço de instalação foi de: ");
    double instalacao = scanner.nextDouble();
    scanner.nextLine();
    System.out.print("\n");

    System.out.println("Escolha o tipo de dispositivo a criar: ");
    System.out.println("1) Smart Bulb");
    System.out.println("2) Smart Camara");
    System.out.println("3) Smart Speaker");
    
    int res = scanner.nextInt();
    scanner.nextLine();
    System.out.print("\n");

        switch(res)
        {
        case 1:
            System.out.print("Tamanho: ");
            double tam= scanner.nextDouble();
            scanner.nextLine();
            System.out.print("\n");

            System.out.print("Consumo: ");
            double consumo= scanner.nextDouble();
            scanner.nextLine();
            System.out.print("\n");

            SmartDevice devBulb = new SmartBulb((id+1),instalacao,tam,consumo);

            suppliers.addSmartDevice(fornecedor,nif,(id+1),room,devBulb);
            break;

        case 2:
            System.out.print("Resolução x: ");
            int x = scanner.nextInt();
            scanner.nextLine();
            System.out.print("\n");

            System.out.print("Resolução y: ");
            int y = scanner.nextInt();
            scanner.nextLine();
            System.out.print("\n");

            System.out.print("Tamanho: ");
            double tamanho= scanner.nextDouble();
            scanner.nextLine();
            System.out.print("\n");

            System.out.print("Consumo: ");
            double consumo2= scanner.nextDouble();
            scanner.nextLine();
            System.out.print("\n");

            SmartDevice devCamera= new SmartCamera((id+1),instalacao,x,y,tamanho,consumo2);

            suppliers.addSmartDevice(fornecedor, nif, (id+1), room, devCamera);
            break;

        case 3:
            System.out.print("Marca: ");
            String marca = scanner.nextLine();
            System.out.print("\n");

            System.out.print("Consumo: ");
            double consumo3 = scanner.nextDouble();
            scanner.nextLine();
            System.out.print("\n");

            SmartDevice devSpeaker = new SmartSpeaker((id+1),instalacao,marca,consumo3);

            suppliers.addSmartDevice(fornecedor, nif, (id+1), room, devSpeaker);
            break;
    }

    return id++;
}

//--------------------------------------------4-----------------------------------------------------//

/**
 * Método que se ocupa de avançar no tempo
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 * @param date var. que tem a data atual
 * @param res var. que a nova data, depois de avançar no tempo
 * @param dias var. que indica quantos dias vão ser avançados
 */
public void MenuTempo(Suppliers suppliers, LocalDate date, LocalDate res, int dias)
{
    System.out.print("-----------MENU TEMPO-----------\n\n");

    Map<Integer,Double> topCasa = new HashMap<>();
    
    System.out.println("O consumo de todas as casas nestes dias foi de: ");
    for(String str:suppliers.getSuppliers().keySet())
    {
        Map<Integer,Double> houseConsumption=new HashMap<>();
        suppliers.getSupplier(str).consumoTotal(houseConsumption);

        Map<Integer,Double> consumptionPrice=new HashMap<>();
        suppliers.getSupplier(str).priceFormula(consumptionPrice,houseConsumption,dias);

        for(Integer str2:houseConsumption.keySet())
        {

            System.out.println("-----------FATURA-----------\n");
            System.out.println("Fornecedor: " + str);
            System.out.println("NIF: " + str2);
            System.out.println("Período de tempo: De " + date + " até " + res);
            System.out.println("Consumo Total: "+ houseConsumption.get(str2));
            System.out.println("Peço Total: " + consumptionPrice.get(str2));
            System.out.print("\n");
            System.out.println("----------------------------");

            suppliers.getSupplier(str).addInvoice(str,str2,dias,houseConsumption.get(str2),consumptionPrice.get(str2));

        }

        System.out.print("\n\n"); 

            int i=-1;
            double valor=0;

            for(Integer str2:consumptionPrice.keySet())
            {
                if(valor<consumptionPrice.get(str2))
                {
                    i=str2;
                    valor=consumptionPrice.get(str2);
                }
            }
            topCasa.put(i,valor); 
    }

    System.out.print("\n");

    System.out.print("Deseja saber qual a casa que gastou mais?[Y/N] ");
        String r = scanner.nextLine();
        
        if(r.equals("Y"))
        {

            int j=-1;
            double maiorValor=0;

            for(Integer str2:topCasa.keySet())
            {
                if(maiorValor<topCasa.get(str2))
                {
                    j=str2;
                    maiorValor=topCasa.get(str2);
                }
            }
            System.out.print("\n");
            System.out.println("A casa que gastou mais foi a de NIF: "+ j);
            System.out.println("Tendo esta gastado: "+ maiorValor);
            System.out.print("\n"); 
            pressEnter();
        }
}

//-------------------------------------------5-----------------------------------------------------//

/**
 * Método que se ocupa de mudar o fornecedor de uma casa
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 * @param consumers lista que tem o consumo de cada fornecedor
 */
public void MenuMudarFornecedor(Suppliers suppliers, List<Consumer<Suppliers>> consumers)
{
    System.out.print("-----------MENU MUDAR FORNECEDOR-----------\n\n");
    System.out.println("Introduza os dados a seguir pedidos.\n");
    
    System.out.println("Lista de fornecedores: ");
    for(String str:suppliers.getSuppliers().keySet())
    {
        System.out.println(str);
    }

    System.out.print("\nDos seguintes fornecedores escolha para o qual quer mudar: ");

    String resSupplier = scanner.nextLine();
    System.out.print("\n");

    Erros erro = new Erros();
    erro.erroExisteFornecedor(suppliers, resSupplier);
    if(erro.getBool()==false) {
        System.out.println(erro);
        return;
    }

    System.out.print("\nIndique o seu fornecedor atual: ");
    String resSupplier2 = scanner.nextLine();
    System.out.print("\n");

    Erros erro2 = new Erros();
    erro2.erroExisteFornecedor(suppliers, resSupplier2);
    if(erro2.getBool()==false) {
        System.out.println(erro2);
        return;
    }
    
    System.out.println("Indique o nif do dono da casa que pretende mudar o seu fornedor: \n");

    for(Integer str:suppliers.getSupplier(resSupplier2).getHouses().keySet())
    {
        System.out.println(str);
    }

    System.out.print("\nnif: ");

    int nif = scanner.nextInt();
    scanner.nextLine();
    System.out.print("\n");

    Erros erro3 = new Erros();
    erro3.erroExisteCasaInteligente(suppliers, resSupplier2, nif);
    if(erro3.getBool()==false) {
        System.out.println(erro3);
        return;
    }

    Consumer<Suppliers> consumer = supplier -> {
        CasaInteligente casa = suppliers.getSupplier(resSupplier2).getHouse(nif).clone();
        suppliers.removeCasaInteligente(resSupplier2, nif);
        suppliers.addCasaInteligente(resSupplier, nif, casa);

        System.out.println("\nFornecedor alterado com sucesso!\n");
    };
    consumers.add(consumer);
    System.out.println(("Modificação de fornecedor guardada com sucesso!\n"));

}

//-------------------------------------------6----------------------------------------------------//

/**
 * Método que se ocupa de ligar ou desligar um dispositivo
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 * @param consumers lista que tem o consumo de cada fornecedor
 */
public void MenuOnOff(Suppliers suppliers, List<Consumer<Suppliers>> consumers)
{
    System.out.print("-----------MENU ON OR OFF-----------\n\n");
    System.out.println("Introduza os dados a seguir pedidos.\n");

    System.out.println("Indique o fornecedor da respetiva casa: ");
    for(String str:suppliers.getSuppliers().keySet())
    {
        System.out.println(str);
    }

    System.out.print("\nFornecedor: ");
    String resSupplier = scanner.nextLine();
    System.out.println("\n");

    Erros erro = new Erros();
    erro.erroExisteFornecedor(suppliers, resSupplier);
    if(erro.getBool()==false) {
        System.out.println(erro);
        return;
    }

    for(Integer str:suppliers.getSupplier(resSupplier).getHouses().keySet())
    {
        System.out.println(str);
    }

    System.out.print("\nIntroduza o nif do dono da casa que pretende ligar/desligar: ");
    int nif = scanner.nextInt();
    scanner.nextLine();

    Erros erro2 = new Erros();
    erro2.erroExisteCasaInteligente(suppliers, resSupplier, nif);
    if(erro2.getBool()==false) {
        System.out.println(erro2);
        return;
    }

    System.out.print("\n");

    System.out.println("Os dispositivos da casa são: ");
    for(SmartDevice entry:suppliers.getSupplier(resSupplier).getHouse(nif).getDevices().values())
    {
    System.out.println(entry);
    }

    System.out.print("Indique o ID do dispositivo a ligar/desligar: ");
    int id=scanner.nextInt();
    scanner.nextLine();
    System.out.print("\n");

    Erros erro3 = new Erros();
    erro3.erroExisteDeviceID(suppliers, resSupplier, nif, id);
    if(erro3.getBool()==false) {
        System.out.println(erro3);
        return;
    }

    System.out.print("Deseja ligar [L] ou desligar [D] o dispositivo: ");
    String r = scanner.nextLine();

    boolean on;

    if(r.equals("L")) on = true;
    else on = false;

    Consumer<Suppliers> consumer = supplier -> {
        supplier.setSmartDeviceOnOff(resSupplier,nif,id,on);
        if(on==true) System.out.println("O dispositivo se encontra ligado");
        else System.out.println("O dispositivo se encontra desligado");
    };

    consumers.add(consumer);
    System.out.println("Modificação guardada com sucesso!\n");

}

//-------------------------------------------7-----------------------------------------------------//

/**
 * Método que se ocupa de mudar o preço de um fornecedor
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 * @param consumers lista que tem o consumo de cada fornecedor
 */
public void MenuPreco(Suppliers suppliers, List<Consumer<Suppliers>> consumers)
{
    System.out.print("-----------MENU PREÇO-----------\n\n");
    System.out.println("Introduza os dados a seguir pedidos.\n");
    
    System.out.println("Indique o fornecedor: ");
    for(String str:suppliers.getSuppliers().keySet())
    {
        System.out.println(str);
    }

    System.out.print("Introduza o fornedor cujo preço quer mudar: ");
    String fornecedor=scanner.nextLine();

    Erros erro = new Erros();
    erro.erroExisteFornecedor(suppliers, fornecedor);
    if(erro.getBool()==false) {
        System.out.println(erro);
        return;
    }

    System.out.print("Introduza o preço que quer: ");
    Double novo = scanner.nextDouble();
    scanner.nextLine();
    System.out.print("\n");

    Erros erro2 = new Erros();
    erro2.erroPriceFornecedor(suppliers, fornecedor, novo);
    if(erro2.getBool()) {
        System.out.println(erro2);
        return;
    }

    Consumer<Suppliers> consumer = supplier -> {
        double antigo = supplier.getPriceEnergySupplier(fornecedor);

            supplier.setPriceEnergySupplier(fornecedor,novo);
            System.out.println("O preço foi alterado de "+antigo+" para "+novo);
    };

    consumers.add(consumer);
    System.out.println("Modificação do preço do fornecedor guardada com sucesso!\n");

}

//-------------------------------------------8-----------------------------------------------------//

/**
 * Método que se ocupa de mudar o imposto de um fornecedor
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 * @param consumers lista que tem o consumo de cada fornecedor
 */
public void MenuTaxa(Suppliers suppliers, List<Consumer<Suppliers>> consumers)
{
    System.out.print("-----------MENU IMPOSTOS-----------\n\n");
    System.out.println("Introduza os dados a seguir pedidos.\n");

    System.out.println("Indique o fornecedor: ");
    for(String str:suppliers.getSuppliers().keySet())
    {
        System.out.println(str);
    }
    
    System.out.print("Introduza o fornedor cujo imposto quer mudar: ");
    String fornecedor=scanner.nextLine();

    Erros erro = new Erros();
    erro.erroExisteFornecedor(suppliers, fornecedor);
    if(erro.getBool()==false) {
        System.out.println(erro);
        return;
    }

    System.out.print("Introduza o imposto que quer: ");
    Double novo = scanner.nextDouble();
    scanner.nextLine();
    System.out.print("\n");

    Erros erro2 = new Erros();
    erro2.erroTaxesFornecedor(suppliers, fornecedor, novo);
    if(erro2.getBool()) {
        System.out.println(erro2);
        return;
    }

    Consumer<Suppliers> consumer = supplier -> {
        double antigo = supplier.getTaxesEnergySupplier(fornecedor);

            supplier.setTaxesEnergySupplier(fornecedor,novo);
            System.out.println("O imposto foi alterado de "+antigo+" para "+novo);
    };

    consumers.add(consumer);
}

//-------------------------------------------9-----------------------------------------------------//

/**
 * Método que se ocupa de calcular o volume de faturação
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 */
public void MenuVolumeFaturacao(Suppliers suppliers)
{
    System.out.print("-----------MENU VOLUME FATURAÇÂO-----------\n\n"); 

    Map<String,Double> volumesFaturacao = new HashMap<>();

    for(String str: suppliers.getSuppliers().keySet())
    {
        double res=suppliers.getSupplier(str).maxValue();
        volumesFaturacao.put(str,res);
    }

    String j="";
    double maiorVolume=0;

    for(String str2:volumesFaturacao.keySet())
    {
        if(maiorVolume<volumesFaturacao.get(str2))
        {
            j=str2;
            maiorVolume=volumesFaturacao.get(str2);
        }
    }
    System.out.print("\n");

    Erros erro = new Erros();
    erro.erroFaturacao(suppliers, j);
    if(erro.getBool()) {
        System.out.println(erro);
        return;
    }

    System.out.println("O fornecedor de energia com maior volume de faturação é o: "+j);
    System.out.println("Tendo como volume: "+ maiorVolume);
}

//-------------------------------------------10----------------------------------------------------//

/**
 * Método que se ocupa de criar as faturas
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 */
public void MenuFaturas(Suppliers suppliers)
{
    System.out.print("-----------MENU FATURAS-----------\n\n");

    System.out.println("Lista de fornecedores: ");
    for(String str:suppliers.getSuppliers().keySet())
    {
        System.out.println(str);
    }

    System.out.print("Introduza o fornecedor do qual quer ver as faturas: ");

    String fornecedor = scanner.nextLine();
    System.out.print("\n");

    Erros erro = new Erros();
    erro.erroExisteFornecedor(suppliers, fornecedor);
    if(erro.getBool()==false) {
        System.out.println(erro);
        return;
    }

    List <Invoice> invoiceList=suppliers.getSupplier(fornecedor).getInvoices();

    for(Invoice temp: invoiceList)
    {
        System.out.println("-----------FATURA-----------\n");
        System.out.println("Fornecedor: " + temp.getSupplier());
        System.out.println("NIF: " + temp.getNif());
        System.out.println("Período de tempo: " + temp.getDays());
        System.out.println("Consumo Total: "+ temp.getTotalConsumption());
        System.out.println("Peço Total: " + temp.getTotalPrice());
        System.out.print("\n");
        System.out.println("----------------------------");
    }
}

//-------------------------------------------11---------------------------------------------------//

/**
 * Método que se ocupa de criar a visualização
 * 
 * @param suppliers var. que tem os fornecedores de energia guardados
 */
public void MenuVisualizar(Suppliers suppliers) {

    String r = "Y";

    while(r.equals("Y")) {

    System.out.print("-----------MENU VISUALIZAR-----------\n\n");
   
    System.out.println("1) Deseja ver o estado de todos os fornecedores \n" +
                       "2) Deseja ver o estado de um fornecedor em especifico. \n" +
                       "3) Deseja ver o estado de uma casa em especifico. \n");

                       
    System.out.print("Introduza a opção: ");
    int opcao = scanner.nextInt();
    scanner.nextLine();
    System.out.print("\n");

    // Estado de todos os fornecedores
    if(opcao == 1)
    {
        System.out.println("-----------ESTADO DOS FORNECEDORES-----------\n");

        for(Map.Entry<String, EnergySupplier> set : suppliers.getSuppliers().entrySet())
        {
            System.out.println(set.getValue().toString() + "N de casas: " + set.getValue().getHouses().size() + " ; ");
        }
    }

    // Estado de um fornecedor em especifico
    if(opcao == 2)
    {
        System.out.println("Indique o fornecedor: ");
        for(String str:suppliers.getSuppliers().keySet())
        {
            System.out.println(str);
        }

        System.out.print("Escolha o fornecedor: ");
        String fornecedor = scanner.nextLine();
        System.out.print("\n");

        Erros erro = new Erros();
        erro.erroExisteFornecedor(suppliers, fornecedor);
        if(erro.getBool()==false) {
            System.out.println(erro);
        }

        if(erro.getBool()==true) {

        EnergySupplier supplier = suppliers.getSupplier(fornecedor);


        System.out.println("\n-----------ESTADO DO FORNECEDOR-----------\n");
        System.out.println("--> " + supplier.toString() + " <--");
        for(Map.Entry<Integer, CasaInteligente> set : supplier.getHouses().entrySet())
        {
            System.out.println("    - Casa de " + '"' + set.getValue().getOwnerName() + '"' + " e o seu nif é '" + set.getValue().getOwnerNIF() + "',");
        }
        }
    }

    // Estado de uma casa em especifico
    if(opcao == 3)
    {
        System.out.println("Indique o fornecedor: ");
        for(String str:suppliers.getSuppliers().keySet())
        {
            System.out.println(str);
        }

        System.out.print("Escolha o fornecedor: ");
        String fornecedor = scanner.nextLine();
        System.out.print("\n");

        Erros erro = new Erros();
        erro.erroExisteFornecedor(suppliers, fornecedor);
        if(erro.getBool()==false) {
            System.out.println(erro);
        }

        if(erro.getBool()==true) {

        System.out.println("Lista dos nifs dos donos das casas: \n");

        for(Map.Entry<Integer, CasaInteligente> entry:suppliers.getSupplier(fornecedor).getHouses().entrySet())
        {
            System.out.println(entry.getValue().toString());
        }
    

        System.out.print("Introduza o nif do dono da casa: ");
        int nif = scanner.nextInt();
        scanner.nextLine();
        System.out.print("\n");

        Erros erro2 = new Erros();
        erro2.erroExisteCasaInteligente(suppliers, fornecedor, nif);
        if(erro2.getBool()==false) {
            System.out.println(erro2);
        }

        if(erro2.getBool()==true) {

        CasaInteligente casa =  suppliers.getSupplier(fornecedor).getHouse(nif);

        System.out.println("\n-----------ESTADO DA CASA ESCOLHIDA-----------\n");
        casa.toString();

        System.out.println("\n-- Devices --\n");

        for(Map.Entry<String, List<Integer>> set : casa.getLocations().entrySet())
        {
            System.out.print("Localização: " + set.getKey() + "\n");
            for(Integer i : set.getValue())
            {
                SmartDevice x = casa.getDevices().get(i);
                System.out.println("    " + x.toString());
            }
            System.out.println("\n");
        }
        }
        }
    }

    System.out.println("\nDeseja continuar no menu de visualização? (Y/N)");
    r = scanner.nextLine();
    System.out.print("\n");
    }
}
}
