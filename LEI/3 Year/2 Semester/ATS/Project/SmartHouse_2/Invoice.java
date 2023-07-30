/**
 * Classe que implementa uma Fatura, compreendendo as devidas
 * declarações de variáveis de instância, a codificação dos construtores, a
 * codificação dos métodos de acesso às variáveis de instância e a codificação
 * de outros métodos que são relevantes para definir o comportamento da Casa
 * Inteligente. 
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class Invoice
{
    private int nif;                    // NIF do Proprietário da Casa Inteligente
    private String supplier;            // Nome do Fornecedor
    private int days;                   // Número de Dias de consumo
    private double totalConsumption;    // Consumo total
    private double totalPrice;          // Preço total do consumo

    /**
     * Construtores da classe Invoice (Fatura)
     */

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public Invoice()
    {
        this.days = 0;
        this.supplier = "";
        this.totalConsumption = 0.0;
        this.totalPrice = 0.0;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param supplier nome do fornecedor
     * @param nif NIF do proprietário
     * @param days numero de dias que foram avançados
     * @param totalConsumption consumo total
     * @param totalPrice preço total
     */
    public Invoice(String supplier, int nif, int days, double totalConsumption, double totalPrice)
    {
        if(nif < 0) this.nif = 0;
        else this.nif = nif;

        this.supplier = supplier;

        if(days < 0) this.days = 0;
        else this.days = days;

        if(totalConsumption < 0) this.totalConsumption = 0.0;
        else this.totalConsumption = totalConsumption;

        if(totalPrice < 0) this.totalPrice = 0.0;
        else this.totalPrice = totalPrice;
    }

    /**
     * Construtor de cópia das variáveis de instância
     * 
     * @param o var. que guarda uma fatura
     */
    public Invoice(Invoice o)
    {
        this.days = o.getDays();
        this.supplier = o.getSupplier();
        this.nif = o.getNif();
        this.totalConsumption = o.getTotalConsumption();
        this.totalPrice = o.getTotalPrice();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o NIF do proprietário da casa associado à fatura
     * 
     * @return NIF do proprietário
     */
    public int getNif()
    {
        return this.nif;
    }

    /**
     * Devolve o fornecedor de energia associado à fatura
     * 
     * @return fornecedor de energia
     */
    public String getSupplier()
    {
        return this.supplier;
    }

    /**
     * Devolve o número de dias de consumo de uma determinada casa associada à fatura
     * 
     * @return número de dias de consumo
     */
    public int getDays() 
    {
        return this.days;
    }

    /**
     * Devolve o consumo total de uma casa associada à fatura
     * 
     * @return consumo total de uma casa
     */
    public double getTotalConsumption()
    {
        return this.totalConsumption;
    }

    /**
     * Devolve o preço total do consumo de uma casa associada à fatura
     * 
     * @return preço total do consumo
     */
    public double getTotalPrice()
    {
        return this.totalPrice;
    }

    /**
     * Atualiza o NIF do proprietário da casa associada à fatura
     * 
     * @param nif novo NIF do proprietário
     */
    public void setNif(int nif) 
    {
        this.nif = nif;
    }

    /**
     * Atualiza o fornecedor de energia de uma casa associada à fatura
     * 
     * @param supplier novo fornecedor de energia
     */
    public void setSupplier(String supplier)
    {
        this.supplier = supplier;
    }

    /**
     * Atualiza o número de dias de consumo de uma casa associada à fatura
     * 
     * @param days novo número de dias de consumo
     */
    public void setDays(int days)
    {
        this.days = days;
    }

    /**
     * Atualiza o consumo total de uma casa associada à fatura
     * 
     * @param totalConsumption novo valor do consumo
     */
    public void setTotalConsumption(double totalConsumption)
    {
        this.totalConsumption = totalConsumption;
    }

    /**
     * Atualiza o preço total do consumo de uma casa associada à fatura
     * 
     * @param totalPrice novo valor do preço
     */
    public void setTotalPrice(double totalPrice)
    {
        this.totalPrice = totalPrice;
    }

     /**
     * Método que faz uma cópia do objeto recetor da mensagem. 
     * Para tal invoca o construtor de cópia
     * 
     * @return objeto clone do objeto que recebe a mensagem
     */
    public Invoice clone()
    {
        return new Invoice(this);
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
        if (!(o instanceof Invoice)) return false;
        Invoice that = (Invoice) o;
        return this.days == that.days && 
                this.totalConsumption == that.totalConsumption && 
                this.totalPrice == that.totalPrice;
    } 

    /**
     * Método que devolve a representação em String da Fatura
     * 
     * @return String com as variáveis de instância da Fatura
     */
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("Invoice{");
        sb.append("ownerNIF='" + nif + '\'');
        sb.append(", supplier=" + supplier);
        sb.append(", days=" + days);
        sb.append(", totalConsumption=" + totalConsumption);
        sb.append(", totalPrice='" + totalPrice + '\'');
        sb.append('}');
        return sb.toString();
    }
}