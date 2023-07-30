/**
 * Classe que implementa uma SmartBulb, compreendendo as devidas
 * declarações de variáveis de instância, a codificação dos construtores, a
 * codificação dos métodos de acesso às variáveis de instância e a codificação
 * de outros métodos que são relevantes para definir o comportamento da Casa
 * Inteligente. Uma SmartBulb é uma lâmpada inteligente que além de ligar e desligar
 * também permite escolher a intensidade da iluminação    
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class SmartBulb extends SmartDevice 
{
    public static final int WARM = 2;        // Maior intensidade da lâmpada
    public static final int NEUTRAL = 1;     // Intensidade Média da lâmpada
    public static final int COLD = 0;        // Intensidade Baixa da lâmpada
    
    private int tone;                        // Intensidade la lâmpada
    private double dimension;                // Dimensão da lâmpada
    private double consumption;              // Consumo base da lâmpada

    /**
     * Construtores da classe SmartBulb
     */

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public SmartBulb() 
    {
        super();
        this.tone = NEUTRAL;
        this.dimension = 0.0;
        this.consumption = 0.0;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id ID do device
     * @param tone tone do device
     * @param dimension dimensao do device
     * @param consumption consumo do device
     */
    public SmartBulb(int id, int tone, double dimension, double consumption) 
    {
        super(id);
        if (tone>WARM) this.tone = WARM;
        else if (tone<COLD) this.tone = COLD;
        else this.tone = tone;
        if(dimension < 0) this.dimension = 0.0;
        else this.dimension = dimension;
        if(consumption < 0) this.consumption = 0.0;
        else this.consumption = consumption;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id ID do device
     * @param instalacao custo de instalacao do device
     * @param tam dimensao do device
     * @param consumption consumo do device
     */
    public SmartBulb(int id, double instalacao, double tam, double consumption) 
    {
        super(id, instalacao);
        this.tone = NEUTRAL;
        this.dimension = tam;
        this.consumption = consumption;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id ID do device
     */
    public SmartBulb(int id) 
    {
        super(id);
        this.tone = NEUTRAL;
        this.dimension = 0.0;
        this.consumption = 0.0;
    }

    /**
     * Construtor de cópia das variáveis de instância
     * 
     * @param o var. que guarda um smartBulb
     */
    public SmartBulb(SmartBulb o) 
    {
        super(o);
        this.tone = o.getTone();
        this.dimension = o.getDimension();
        this.consumption = o.getConsumption();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve a intensidade da SmartBulb
     * 
     * @return intensidade
     */
    public int getTone() 
    {
        return this.tone;
    }

    /**
     * Devolve a dimensão da SmartBulb
     * 
     * @return dimensão
     */
    public double getDimension() 
    {
        return this.dimension;
    }

    /**
     * Devolve o consumo base da SmartBulb
     * 
     * @return consumo base
     */
    public double getConsumption() 
    {
        return this.consumption;
    }

    /**
     * Atualiza a intensidade da SamrtBulb
     * 
     * @param tone nova intensidade
     */
    public void setTone(int tone) 
    {
        this.tone = tone;
    }

    /**
     * Atualiza a dimensão da SmartBulb
     * 
     * @param dimension nova dimensão
     */
    public void setDimension(double dimension) 
    {
        this.dimension = dimension;
    }

    /**
     * Atualiza o consumo base da lâmpada
     * 
     * @param consumption novo consumo base
     */
    public void setConsumption(double consumption) 
    {
        this.consumption = consumption;
    }
    
    /**
     * Método que calcula o consumo diario da SmartBulb
     * 
     * @return consumo diário
     */
    public double dailyConsumption()
    {
        if(this.getOn()==false) return 0;
        else return consumption + this.tone * 0.1;
    }

    /**
     * Método que faz uma cópia do objeto recetor da mensagem. 
     * Para tal invoca o construtor de cópia
     * 
     * @return objeto clone do objeto que recebe a mensagem
     */
    public SmartBulb clone()
    {
        return new SmartBulb(this);
    }

    /**
     * Método equals que todos os objetos possuem
     * 
     * @param * objeto é comparado com o recetor da mensagem
     * @return verdadeiro ou falso
     */
    public boolean equals(Object object) 
    {
        if (this == object) return true;
        if (!(object instanceof SmartBulb)) return false;
        if (!super.equals(object)) return false;
        SmartBulb smartBulb = (SmartBulb) object;
        return this.tone == smartBulb.tone &&
                this.dimension == smartBulb.dimension &&
                this.consumption == smartBulb.consumption;
    }

    /**
     * Método que devolve a representação em String da SmartBulb
     * 
     * @return String com as variáveis de instância da SmartBulb
     */
    public String toString() 
    {
        return "SmartBulb{" + super.toString() + ", tone=" + tone + ", dimension=" + dimension + ", consumption=" + consumption + '}';
    }  
}
