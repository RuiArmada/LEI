import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * Classe que implementa um Fornecedor de Energia, compreendendo as devidas
 * declarações de variáveis de instância, a codificação dos construtores, a
 * codificação dos métodos de acesso às variáveis de instância e a codificação
 * de outros métodos que são relevantes para definir o comportamento da Casa
 * Inteligente. Além disso, faz a gestão das Casas que estão no seu domínio    
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class EnergySupplier 
{
    private String name;                            // Nome do Fornecedor
    private double price;                           // Valor base do custo da Energia
    private double taxes;                           // Impostos aplicados
    private Map <Integer, CasaInteligente> houses;  // NIF, Lista de Casas Inteligentes
    private List <Invoice> invoicesList;            // Lista de Faturas


    /**
     * Construtores da classe Energy Supplier (Fornecedor de Energia)
     */

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public EnergySupplier() 
    {
        this.name = "";
        this.price = 0.0;
        this.taxes = 0.23;
        this.houses = new HashMap<Integer, CasaInteligente>(); 
        this.invoicesList = new ArrayList<Invoice>();
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param name nome do fornecedor
     * @param price preço do fornecedor
     */    
    public EnergySupplier(String name, double price)
    {
        this.name = name;
        if(price < 0) this.price = 0.0;
        else this.price = price;
        this.taxes = 0.23;
        this.houses = new HashMap<Integer, CasaInteligente>(); 
        this.invoicesList = new ArrayList<Invoice>();
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param name nome do fornecedor
     */ 
    public EnergySupplier(String name) 
    {
        this.name = name;
        this.price = name.length();
        this.taxes = 0.23;
        this.houses = new HashMap<Integer, CasaInteligente>(); 
        this.invoicesList = new ArrayList<Invoice>();
    }

    /**
     * Construtor de cópia das variáveis de instância
     * 
     * @param o var. que guarda um fornecedor
     */
    public EnergySupplier(EnergySupplier o) 
    {
        this.name = o.getName();
        this.price = o.getPrice();
        this.taxes = o.getTaxes();
        this.houses = o.getHouses();
        this.invoicesList = o.getInvoices();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o nome do fornecedor de energia
     * 
     * @return nome do fornecedor de energia
     */
    public String getName() 
    {
        return this.name;
    }

    /**
     * Devolve o valor base do custo de energia diário do fornecedor
     * 
     * @return valor base do custo de energia
     */
    public double getPrice() 
    {
        return this.price;
    }

    /**
     * Devolve o valor dos impostos aplicados pelo fornecedor
     * 
     * @return valor dos impostos
     */
    public double getTaxes() 
    {
        return this.taxes;
    }

    /**
     * Devolve a lista de casas que estão no domínio do fornecedor em questão
     * 
     * @return lista de casas
     */
    public Map <Integer, CasaInteligente> getHouses() {
        return this.houses;
    }

    /**
     * Devolve a lista de faturas associadas às casas que estão no domínio do fornecedor
     * 
     * @return lista de faturas
     */
    public List <Invoice> getInvoices()
    {
        return this.invoicesList;
    }

    /**
     * Devolve a casa referente ao ID em questão
     * 
     * @param id ID da casa, neste caso o NIF do proprietário
     * 
     * @return Casa Inteligente referente ao ID
     */
    public CasaInteligente getHouse(int id) 
    {
        return this.houses.get(id).clone();
    }

    /**
     * Atualiza o nome do fornecedor
     * 
     * @param name novo nome do fornecedor
     */
    public void setName(String name) 
    {
        this.name = name;
    }

    /**
     * Atualiza o preço base do custo de energia do fornecedor
     * 
     * @param price novo valor base do custo de energia
     */
    public void setPrice(double price) 
    {
        this.price = price;
    }

    /**
     * Atualiza os impostos aplicados pelo fornecedor
     * 
     * @param taxes novo valor dos impostos
     */
    public void setTaxes(double taxes) 
    {
        this.taxes = taxes;
    }   

    /**
     * Atualiza a lista de casas associadas ao fornecedor em questão
     * 
     * @param houses nova lista de casas
     */
    public void setHouses(Map <Integer, CasaInteligente> houses) 
    {
        this.houses = houses;
    }

    /**
     * Atualiza a lista de faturas associadas às casas do fornecedor
     * 
     * @param invoices nova lista de faturas
     */
    
    public void setInvoicesList(ArrayList<Invoice> invoices)
    {
        this.invoicesList = invoices;
    }

    /**
     * Método que devolve o consumo diário consoante o fornecedor
     * 
     * @return valor do consumo diário
     */
    public double dailyConsumption() 
    {
        int l = this.name.length();
        if(l>=10) return l*0.1;
        else return l*0.2;
    }

    /**
     * Método que adiciona uma casa ao fornecedor em questão
     * 
     * @param dono Nome do proprietário da casa
     * @param nif NIF do proprietário da casa
     * @param supplier Nome do fornecedor de energia
     */
    public void addCasaInteligente(String dono, int nif, String supplier) 
    {
        CasaInteligente casa = new CasaInteligente(dono, nif, supplier);
        this.houses.put(nif, casa);
    }

    /**
     * Método que adiciona uma casa
     * 
     * @param nif NIF do proprietario da casa
     * @param casa Casa a adicionar
     * @param supplier Nome do fornecedor
     */
    public void addCasaInteligente(int nif, CasaInteligente casa, String supplier) 
    {
        casa.setSupplier(supplier);
        this.houses.put(nif,casa);
    }

    /**
     * Método que remove uma casa
     * 
     * @param nif NIF da casa a remover
     */
    public void removeCasaInteligente(int nif) 
    {
        this.houses.remove(nif);
    }

    /**
     * Método que adiciona um dispositivo a uma divisão a uma casa do fornecedor
     * 
     * @param nif NIF do proprietário da casa
     * @param id ID do dispositivo
     * @param room Divisão da casa
     * @param smartDevice Dispositivo a adicionar
     */
    public void addSmartDevice(int nif, int id, String room, SmartDevice smartDevice) 
    {
        this.houses.get(nif).addSmartDevice(id, room, smartDevice);
    }
    
    /**
     * Método que adiciona os dados a uma fatura, de uma determinada casa do fornecedor
     * 
     * @param supplier Nome do fornecedor
     * @param nif NIF do proprietário da casa
     * @param days Número de dias a incluir na fatura
     * @param totalConsumption consumo total da casa
     * @param totalPrice preço total da energia
     */
    public void addInvoice(String supplier, int nif, int days, double totalConsumption, double totalPrice)
    {
        Invoice invoice = new Invoice(supplier, nif, days,totalConsumption,totalPrice);
        this.invoicesList.add(invoice);
    }

    /**
     * Método que permite calcular o consumo total de todas as casas
     * 
     * @param houseConsumption listagem do consumo de todas as casas
     */
    public void consumoTotal (Map<Integer,Double> houseConsumption)
    {
        for(Map.Entry<Integer, CasaInteligente> entry : this.houses.entrySet()) 
        {
            houseConsumption.put(entry.getKey(), entry.getValue().getTotalConsumption());
        }
    }

    /**
     * Método que determina a fórmula de cálculo do preço da energia cobrado por um determinado fornecedor
     * 
     * @param housePrice preço do consumo de cada casa
     * @param houseConsumption consumo de cada casa
     * @param dias número de dias do consumo
     */
    public void priceFormula(Map<Integer,Double> housePrice, Map<Integer,Double> houseConsumption, int dias)
    {
        double baseValue = this.dailyConsumption();
        for(Map.Entry<Integer, CasaInteligente> entry : this.houses.entrySet()) 
        {
            double deviceConsumption = houseConsumption.get(entry.getKey());
            double deviceDayConsPrice = (baseValue + deviceConsumption * (1+this.taxes)) * 0.9;
            housePrice.put(entry.getKey(), (deviceDayConsPrice*dias));
        }
    }

    /**
     * Método que determina o valor máximo de todas as faturas associadas ao fornecedor
     * 
     * @return valor máximo cobrado
     */
    public double maxValue() 
    {
        double max = 0;
        for(Invoice invoice : this.invoicesList) 
        {
            if(invoice.getTotalPrice() > max) max = invoice.getTotalPrice();
        }
        return max;
    }

    /**
     * Método que permite ligar e desligar um dispositivo de uma determinada casa
     * 
     * @param *NIF NIF do proprietario da casa
     * @param *id ID do dispositivo a ligar/desligar
     * @param *on ligar/desligar dispositivo
     */
    public void setSmartDeviceOnOff(int nif, int id, boolean on)
    {
        this.houses.get(nif).setSmartDeviceOnOff(id, on);
    }

    /**
     * Método que devolve a representação em String do Fornecedor de Energia
     * 
     * @return String com as variáveis de instância do Fornecedor de Energia
     */
    public String toString() 
    {
        return "[Fornecedor " + name + " ] : Preço : " + price + " | Taxas : " + taxes + " ; ";
    }

    /**
     * Método equals que todos os objetos possuem
     * 
     * @param o o objeto é comparado com o recetor da mensagem
     * @return verdadeiro ou falso
     */
    public boolean equals(Object o) 
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EnergySupplier that = (EnergySupplier) o;
        return this.houses.equals(that.houses) &&
                this.invoicesList.equals(that.invoicesList) && 
                this.name.equals(that.name) && 
                this.taxes == that.taxes && 
                this.price == that.price;
    }

    /**
     * Método que faz uma cópia do objeto recetor da mensagem. 
     * Para tal invoca o construtor de cópia
     * 
     * @return objeto clone do objeto que recebe a mensagem
     */
    public EnergySupplier clone() 
    {
        return new EnergySupplier(this);
    } 
}
