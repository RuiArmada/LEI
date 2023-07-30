/**
 * Classe que implementa uma SmartCamera, compreendendo as devidas
 * declarações de variáveis de instância, a codificação dos construtores, a
 * codificação dos métodos de acesso às variáveis de instância e a codificação
 * de outros métodos que são relevantes para definir o comportamento da Casa
 * Inteligente. Uma SmartBulb é uma camara que filma vídeos, além de ligar e 
 * desligar, tendo ainda associada uma resolução e um tamanho dos ficheiros gerados   
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class SmartCamera extends SmartDevice 
{
    private static final int MIN = 0;       // Tamanho mínimo dos ficheiros

    private int x;                  // Primeira variável da resolução
    private int y;                  // Segunda variável da resolução
    private double tamanho;         // Tamanho do ficheiro gerado
    private double consumption;     // Consumo base da SmartCamera

    /**
     * Construtores da classe SmartCamera
     */

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public SmartCamera() 
    {
        super();
        this.x = 0;
        this.y = 0;
        this.tamanho = 0.0;
        this.consumption = 0.0;
        
    }
    
    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id ID do device
     * @param x resolução x 
     * @param y resolução y
     * @param tamanho tamanho do ficheiro gerado
     */
    public SmartCamera(int id, int x, int y, double tamanho) 
    {
        super(id);

        if(x<MIN) this.x = 0;
        else this.x = x;

        if(y<MIN) this.y = 0;
        else this.y = y;     

        if(tamanho<MIN) this.tamanho = 0;
        else this.tamanho = tamanho;

        if(consumption<0) this.consumption = 0;
        else this.consumption = this.dailyConsumption();
    }

   /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id ID do device
     * @param x resolução x 
     * @param y resolução y
     * @param tamanho tamanho do ficheiro gerado
     * @param consumption consumo base da SmartCamera
     */
    public SmartCamera(int id, int x, int y, double tamanho, double consumption) 
    {
        super(id);

        if(x<MIN) this.x = 0;
        else this.x = x;

        if(y<MIN) this.y = 0;
        else this.y = y;      

        if(tamanho<MIN) this.tamanho = 0;
        else this.tamanho = tamanho;

        if(consumption<0) this.consumption = 0;
        else this.consumption = consumption;
    }

   /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id ID do device
     * @param instalacao custo de instalação do device
     * @param x resolução x 
     * @param y resolução y
     * @param tamanho tamanho do ficheiro gerado
     * @param consumption consumo base da SmartCamera
     */
    public SmartCamera(int id, double instalacao, int x, int y, double tamanho, double consumption) 
    {
        super(id, instalacao);

        if(x<MIN) this.x = 0;
        else this.x = x;

        if(y<MIN) this.y = 0;
        else this.y = y;    

        if(tamanho<MIN) this.tamanho = 0;
        else this.tamanho = tamanho;

        if(consumption<0) this.consumption = 0;
        else this.consumption = consumption;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param id id do device
     */
    public SmartCamera(int id) 
    {
        super(id);
        this.x = 0;
        this.y = 0;
        this.tamanho = 0.0;
        this.consumption = 0.0;
    }

    /**
     * Construtor de cópia das variáveis de instância
     * 
     * @param o var. que guarda uma smartCamera
     */
    public SmartCamera(SmartCamera o) 
    {
        super(o);
        this.x = o.getX();
        this.y = o.getY();
        this.tamanho = o.getTamanho();
        this.consumption = o.getConsumption();
    }

    /**
     * Métodos de Instância
     */

     /**
      * Devolve a primeira variável da resolução da SmartCamera
      *
      * @return primeira variável da resolução
      */
    public int getX() 
    {
        return this.x;
    }  

    /**
     * Devolve a segunda variável da resolução da SmartCamera
     * 
     * @return segunda variável da resolução
     */
    public int getY()
    {
        return this.y;
    }

    /**
     * Devolve o tamanho do ficheiro gerado pela SmartCamera
     * 
     * @return tamanho do ficheiro
     */
    public double getTamanho() 
    {
        return this.tamanho;
    }

    /**
     * Devolve o valor do consumo base da SmartCamera
     * 
     * @return valor de consumo base
     */
    public double getConsumption() 
    {
        return this.consumption;
    }

    /**
     * Atualiza o valor da primeira variável da resolução
     * 
     * @param x novo valor da primeira variável da resolução
     */
    public void setX(int x) 
    {
        this.x = x;
    }

    /**
     * Atualiza o valor da segunda variável da resolução
     * 
     * @return valor da segunda variável da resolução
     */
    public void setY(int y) 
    {
        this.y = y;
    }

    /**
     * Atualiza o valor do tamamnho do ficheiro gerado pela SmartCamera
     * 
     * @param tamanho novo tamanho do ficheiro
     */
    public void setTamanho(int tamanho) 
    {
        this.tamanho = tamanho;
    }

    /**
     * Atualiza o valor do consumo base da SmartCamera
     * 
     * @param consumption novo valor do consumo base
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
        else return this.tamanho*this.x*this.y*0.000000001 + consumption;
    }

    /**
     * Método que faz uma cópia do objeto recetor da mensagem. 
     * Para tal invoca o construtor de cópia
     * 
     * @return objeto clone do objeto que recebe a mensagem
     */
    public SmartCamera clone()
    {
        return new SmartCamera(this);
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
        if (!(object instanceof SmartCamera)) return false;
        if (!super.equals(object)) return false;
        SmartCamera smartCamera = (SmartCamera) object;
        return this.x == smartCamera.x && 
                this.y == smartCamera.y && 
                this.tamanho == smartCamera.tamanho;
    }

    /**
     * Método que devolve a representação em String da SmartCamera
     * 
     * @return String com as variáveis de instância da SmartCamera
     */
    public String toString() 
    {
        return "SmartCamera{" + super.toString() + ", x=" + x + ", y=" + y + ", tamanho=" + tamanho + ", consumption=" + consumption + '}';
    }
}
