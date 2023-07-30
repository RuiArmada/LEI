import java.util.HashMap;
import java.util.Map;

/**
 * Classe que implementa o conjunto de fornecedores existentes, compreendendo 
 * as devidas declarações de variáveis de instância, a codificação dos construtores,
 * a codificação dos métodos de acesso às variáveis de instância e a codificação
 * de outros métodos que são relevantes para definir o comportamento da Casa
 * Inteligente.
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class Suppliers 
{
    private Map<String, EnergySupplier> suppliers; // Listagem de Fornecedores

    /**
     * Construtores da classe Energy Supplier (Fornecedor de Energia)
     */

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public Suppliers() 
    {
        this.suppliers = new HashMap<String, EnergySupplier>();
    }

    /**
     * Construtor de inicialização das variáveis de instância
     * 
     * @param o var. que garda um Suppliers
     */
    public Suppliers(Suppliers o) 
    {
        this.suppliers = o.getSuppliers();
    }

    /**
     * Construtor de inicialização das variáveis de instância
     * 
     * @param suppliers var. que guarda um map de fornecedores
     */
    public Suppliers (Map<String, EnergySupplier> suppliers) {
        this.suppliers = suppliers;
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve a lista de fornecedores
     * 
     * @return lista de fornecedores
     */
    public Map<String, EnergySupplier> getSuppliers() 
    {
        return this.suppliers;
    }

    /**
     * Atualiza a lista de fornecedores
     * 
     * @param suppliers nova lista de fornecedores
     */
   
    public void setSuppliers(Map<String, EnergySupplier> suppliers) 
    {
        this.suppliers = suppliers;
    }

     /**
     * Método que devolve o fornecedor correspondente ao nome fornecido
     * 
     * @param name nome do fornecedor a pesquisar
     * 
     * @return fornecedor pretendido
     */
    public EnergySupplier getSupplier(String name) 
    {
        return this.suppliers.get(name);
    }

    /**
     * Método que adiciona um fornecedor e o seu preço à lista de fornecedores
     * 
     * @param name nome do fornecedor
     * @param price preço do fornecedor
     */
    public void addEnergySupplier(String name, double price) 
    {
         this.suppliers.put(name, new EnergySupplier(name, price));
    }

    /**
     * Método que adiciona um fornecedor através do nome à lista de fornecedores
     * 
     * @param name nome do fornecedor
     */
    public void addEnergySupplier(String name) 
    {
        this.suppliers.put(name, new EnergySupplier(name));
    }

    /**
     * Método que adiciona um fornecedor à lista de fornecedores
     * 
     * @param supplier fornecedor a adicionar
     */
    public void addEnergySupplier(EnergySupplier supplier) 
    {
        this.suppliers.put(supplier.getName(), supplier);
    }

    /**
     * Método que adiciona uma Casa Inteligente a um determinado fornecedor
     * 
     * @param owner nome do proprietário
     * @param nif NIF do proprietário
     * @param supplier nome do fornecedor
     */
    public void addCasaInteligente(String owner, int nif, String supplier) 
    {
        this.suppliers.get(supplier).addCasaInteligente(owner, nif, supplier);
    }

    /**
     * Método que adiciona uma Casa Inteligente a um fornecedor
     * 
     * @param supplier fornecedor de energia
     * @param nif NIF do proprietário
     * @param casa Casa a adicionar
     */
    public void addCasaInteligente(String supplier, int nif, CasaInteligente casa) 
    {
        this.suppliers.get(supplier).addCasaInteligente(nif, casa, supplier);
    }

    /**
     * Método que remove uma Casa Inteligente de um fornecedor
     * 
     * @param supplier fornecedor de energia
     * @param nif NIF do proprietário
     */
    public void removeCasaInteligente(String supplier, int nif) 
    {
        this.suppliers.get(supplier).removeCasaInteligente(nif);
    }

    /**
     * Método que adiciona um SmartDebive a uma determinada casa, divisão de um certo fornecedor
     * 
     * @param supplier nome do fornecedor
     * @param nif NIF do proprietário
     * @param id ID do dispositivo
     * @param room divisão da casa
     * @param s dispositivo a adicionar
     */
    public void addSmartDevice(String supplier, int nif, int id, String room, SmartDevice s) 
    {
        this.suppliers.get(supplier).addSmartDevice(nif, id, room, s);
    }

    /**
     * Método que permite ligar e desligar um determinado dispositivo
     * 
     * @param supplier fornecedor de energia
     * @param nif NIF do proprietário
     * @param id ID do dispositivo
     * @param on ligar/desligar
     */
    public void setSmartDeviceOnOff(String supplier, int nif, int id, boolean on) 
    {
        this.suppliers.get(supplier).setSmartDeviceOnOff(nif, id, on);
    }

    /**
     * Método que permite obter o preço associado a um fornecedor de energia
     * 
     * @param supplier fornecedor de energia
     * @return preço da energia
     */
    public double getPriceEnergySupplier(String supplier) 
    {
        return this.suppliers.get(supplier).getPrice();
    }

    /**
     * Método que permite atualizar o preço associado a um fornecedor de energia
     * 
     * @param supplier fornecedor de energia
     * @param price novo preço
     */
    public void setPriceEnergySupplier(String supplier, double price) 
    {
        this.suppliers.get(supplier).setPrice(price);
    }

    /**
     * Método que permite obter os impostos cobrados pelo fornecedor de energia
     * 
     * @param supplier fornecedor de energia
     * @return valor dos impostos
     */
    public double getTaxesEnergySupplier(String supplier) 
    {
        return this.suppliers.get(supplier).getTaxes();
    }

    /**
     * Método que permite atualizar os impostos cobrados pelo fornecedor de energia
     * 
     * @param supplier fornecedor de energia
     * @param taxes novo valor dos impostos
     */
    public void setTaxesEnergySupplier(String supplier, double taxes) 
    {
        this.suppliers.get(supplier).setTaxes(taxes);
    }

    /**
     * Método que faz uma cópia do objeto recetor da mensagem. 
     * Para tal invoca o construtor de cópia
     * 
     * @return objeto clone do objeto que recebe a mensagem
     */
    public Suppliers clone()
    {
        return new Suppliers(this);
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
        if (!(o instanceof Suppliers)) return false;
        if (!super.equals(o)) return false;
        Suppliers that = (Suppliers) o;
        return that.suppliers.equals(this.suppliers);
    }

    /**
     * Método que devolve a representação em String da Lista de Fornecedores
     * 
     * @return String com as variáveis de instância da Lista de Fornecedores
     */
    public String toString()
    {
       return suppliers.toString();
    }
}
