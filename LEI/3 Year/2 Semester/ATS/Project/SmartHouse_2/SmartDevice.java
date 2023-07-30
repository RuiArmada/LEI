/**
 * Classe que implementa um SmartDevice, compreendendo as devidas
 * declarações de variáveis de instância, a codificação dos construtores, a
 * codificação dos métodos de acesso às variáveis de instância e a codificação
 * de outros métodos que são relevantes para definir o comportamento da Casa
 * Inteligente. Um SmartDevice é um contactor simples que permite ligar ou
 * desligar circuitos
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public abstract class SmartDevice 
{
    private int id;                     // ID do dispositivo
    private boolean on;                 // Estado do dispositivo (ON/OFF)
    private double installationCost;    // Custo de Instalação

    /**
     * Construtores da classe SmartDevice
     */

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public SmartDevice() 
    {
        this.id = 0;
        this.on = false;
        this.installationCost = 0.0;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id id do device
     */
    public SmartDevice(int id) 
    {
        this.id = id;
        if(id%2 == 0) this.on = true;
        else this.on = false;
        this.installationCost = 0.0;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param *id id do device
     * @param *on estado do device
     */
    public SmartDevice(int s, boolean b) 
    {
        this.id = s;
        this.on = b;
        this.installationCost = 0.0;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param *id id do device
     * @param *cost custo de instalação
     */
    public SmartDevice(int s, double cost) 
    {
        this.id = s;
        if(s%2 == 0) this.on = true;
        else this.on = false;
        this.installationCost = cost;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     *
     * @param installationCost custo de instalação
     */
    public SmartDevice(double installationCost) 
    {
        this.id = 0;
        this.on = true;
        this.installationCost = installationCost;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param *id id do device
     * @param *on estado do device
     * @param installationCost custo de instalação
     */
    public SmartDevice(int s, boolean b, double installationCost) 
    {
        this.id = s;
        this.on = b;
        this.installationCost = installationCost;
    }


    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param o var. que guarda um smartDevice
     */
    public SmartDevice(SmartDevice o) 
    {
        this.id = o.getID();
        this.on = o.getOn();
        this.installationCost = o.getInstallationCost();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o estado do SmartDevice (ligado ou desligado)
     * 
     * @return estado do SmartDevice
     */
    public boolean getOn() 
    {
        return this.on;
    }

    /**
     * Devolve o ID do SmartDevice
     * 
     * @return ID do SmartDevice
     */
    public int getID() 
    {
        return this.id;
    }

    /**
     * Devolve o custo de instalação do SmartDevice
     * 
     * @return custo de instalação
     */
    public double getInstallationCost() 
    {
        return this.installationCost;
    }

    /**
     * Atualiza o estado do SmartDevice
     * 
     * @param bool novo estado
     */
    
    public void setOn(boolean bool) 
    { 
        this.on = bool;
    }
    
    /**
     * Atualiza o ID do SmartDevice
     * 
     * @param id ID do SmartDevice
     */
    public void setID(int id)
    {
        this.id = id;
    }
    
    /**
     * Atualiza o custo de instalação do SmartDevice
     * 
     * @param installationCost novo custo de instalação
     */
    public void setInstallationCost(double installationCost) 
    {
        this.installationCost = installationCost;
    }

    /**
     * Método que liga o SmartDevice
     */
    public void turnOn() 
    {
        this.on = true;
    }
    
    /**
     * Método que desliga o SmartDevice
     */
    public void turnOff() 
    {
        this.on = false;
    }

    /**
     * Método que calcula o consumo diário do dispositivo
     * 
     * @return consumo diário
     */
    public abstract double dailyConsumption();
    
    /**
     * Método que faz uma cópia do objeto recetor da mensagem. 
     * Para tal invoca o construtor de cópia
     * 
     * @return objeto clone do objeto que recebe a mensagem
     */
    public abstract SmartDevice clone();

    /**
     * Método equals que todos os objetos possuem
     * 
     * @param *o o objeto é comparado com o recetor da mensagem
     * @return verdadeiro ou falso
     */
    public boolean equals(Object object) 
    {
        if (this == object) return true;
        if (!(object instanceof SmartDevice)) return false;
        SmartDevice that = (SmartDevice) object;
        return this.on == that.on && 
                this.id == that.id;
    }

    /**
     * Método que devolve a representação em String do SmartDevice
     * 
     * @return String com as variáveis de instância do SmartDevice
     */
    public String toString() 
    {
        return " id = " + id + " | on = " + on + " | installationCost = " + installationCost;
    }
}
