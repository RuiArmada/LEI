/**
 * Classe que implementa uma SmartSpeaker, compreendendo as devidas
 * declarações de variáveis de instância, a codificação dos construtores, a
 * codificação dos métodos de acesso às variáveis de instância e a codificação
 * de outros métodos que são relevantes para definir o comportamento da Casa
 * Inteligente. Uma SmartSpeaker é um SmartDevice que além de ligar e desligar
 * permite também reproduzir som. Consegue-se ligar a um canal (rádio online)
 * e permite a regulação do seu nível de volume.    
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class SmartSpeaker extends SmartDevice 
{
    public static final int MAX = 20; // Volume Máximo
    
    private int volume;               // Volume da SmartSpeaker
    private String channel;           // Canal de rádio
    private String brand;             // Marca da SmartSpeaker
    private double consumption;       // Consumo base da SmartSpeaker

    /**
     * Construtores da classe SmartSpeaker
     */

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public SmartSpeaker() 
    {
        super();
        this.volume = 0;
        this.channel = "";
        this.brand = "";
        this.consumption = 0.0;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param channel canal de rádio
     * @param brand marca da SmartSpeaker
     */
    public SmartSpeaker(String channel, String brand) 
    {
        super();
        this.volume = 10;
        this.channel = channel;
        this.brand = brand;
        this.consumption = 0.0;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id id do device
     * @param instalacao custo de instalacao do device
     * @param brand marca da SmartSpeaker
     * @param consumption consumo do device
     */
    public SmartSpeaker(int id, double instalacao, String brand, Double consumption) 
    {
        super(id, instalacao);
        this.volume = 0;
        this.channel = "";
        this.brand = brand;
        this.consumption = consumption;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id id do device
     * @param channel canal de rádio
     * @param brand marca da SmartSpeaker
     * @param volume volume da SmartSpeaker
     * @param consumption consumo do device
     */
    public SmartSpeaker(int id, String channel, String brand, int volume, double consumption) 
    {
        super(id);

        if(volume < 0 || volume > MAX) this.volume = 0;
        else this.volume = volume;
        
        this.channel = channel;
        this.brand = brand;

        if(consumption < 0) this.consumption = 0;
        else this.consumption = consumption;
    }

    /**
     * Construtor de cópia das variáveis de instância
     * 
     * @param o var. que guarda um SmartSpeaker
     */
    public SmartSpeaker(SmartSpeaker o)
    {
        super(o);
        this.volume = o.getVolume();
        this.channel = o.getChannel();
        this.brand = o.getBrand();
        this.consumption = o.getConsumption();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve a marca da SmartSpeaker
     * 
     * @return marca
     */
    public String getBrand() 
    {
        return this.brand;
    }
   
    /**
     * Devolve o volume da SmartSpeaker
     * 
     * @return volume
     */
    public int getVolume() 
    {
        return this.volume;
    }

    /**
     * Devolve o canal de rádio da SmartSpeaker
     * 
     * @return canal de rádio
     */
    public String getChannel() 
    {
        return this.channel;
    }

    /**
     * Devolve o valor do consumo base da SmartSpeaker
     * 
     * @return valor do consumo base
     */
    public double getConsumption() 
    {
        return this.consumption;
    }

    /**
     * Atualiza a marca da SmartSpeaker
     * 
     * @param brand nova marca
     */
    public void setBrand(String brand) 
    {
        this.brand = brand;
    }

    /**
     * Atualiza o volume da SmartSpeaker
     * 
     * @param newVolume novo volume
     */
    public void setVolume(int newVolume) 
    {
        this.volume = newVolume;
    }

    /**
     * Atualiza o canal de rádio da SmartSpeaker
     * 
     * @param newChannel novo canal
     */
    public void setChannel(String newChannel) 
    { 
        this.channel = newChannel;
    }

    /**
     * Atualiza o valor do consumo base da SmartSpeaker
     * 
     * @param consumption novo valor de consumo base
     */
    public void setConsumption(double consumption) 
    {
        this.consumption = consumption;
    }

    /**
     * Método que aumenta o volume em uma unidade
     */
    public void volumeUp() 
    {
        if (this.volume<MAX) this.volume++;
    }

    /**
     * Método que diminui o volume em uma unidade
     */
    public void volumeDown() 
    {
        if (this.volume>0) this.volume--;
    }

    /**
     * Método que calcula o consumo diario da SmartBulb
     * 
     * @return consumo diário
     */
    public double dailyConsumption()
    {
        if(this.getOn()==false) return 0;
        else return consumption + 0.02*this.volume;
    }
    
    /**
     * Método que faz uma cópia do objeto recetor da mensagem. 
     * Para tal invoca o construtor de cópia
     * 
     * @return objeto clone do objeto que recebe a mensagem
     */
    public SmartSpeaker clone()
    {
        return new SmartSpeaker(this);
    }

    /**
     * Método equals que todos os objetos possuem
     * 
     * @param *o o objeto é comparado com o recetor da mensagem
     * @return verdadeiro ou falso
     */
    public boolean equals(Object object) 
    {
        if (this == object) return true;
        if (!(object instanceof SmartSpeaker)) return false;
        if (!super.equals(object)) return false;
        SmartSpeaker that = (SmartSpeaker) object;
        return this.volume == that.volume && 
                this.channel == that.channel &&
                this.brand == that.brand &&
                this.consumption == that.consumption;
    }

    /**
     * Método que devolve a representação em String da SmartSpeaker
     * 
     * @return String com as variáveis de instância da SmartSpeaker
     */
    public String toString()
    {
        return "SmartSpeaker{" + super.toString() + ", volume=" + volume + ", channel=" + channel + ", brand=" + brand + ", consumption=" + consumption + '}';
    }
}
