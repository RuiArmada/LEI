package smart_houses.smart_devices;


public class SmartBulb extends SmartDevice {
  // Tipo enum que representa os varios tipos de tons da lampada, juntamento com o concumo associado a casa um
    public enum Tones{
        NEUTRAL(0.03), WARM(0.05), COLD(0.02);

// Variavel de instancia que guarda o valor do consumo associado ao tom de luz
        private final double consume;

//Construtor parametrizado
        Tones(double consume){
            this.consume = consume;
        }

        /**
         * Metodo que retorna o consumo associado ao tom atual
         * 
         * @return valor do consumo associado ao tom de luz
         */
        public double getConsume(){
            return this.consume;
        }
    }

    // tom de luz da lampada
    private Tones tone;
    // dimensao da lampada
    private final int dimension;

// Construtor por omissao
    public SmartBulb(){
        super();
        this.tone = Tones.NEUTRAL;
        this.dimension = 20;
    }

// Construtor parametrizado
    public SmartBulb(boolean on, double consume, Tones tone, int dimension){
        super(on, consume);
        this.tone = tone;
        this.dimension = dimension;
    }

    // Contrutor de copia
    public SmartBulb(SmartBulb device){
        super(device);
        this.tone = device.getTone();
        this.dimension = device.getDimension();
    }

    /**
     * Metodo que retorna o tom de luz da lampada
     * 
     * @return Valor do tom de luz da lampada
     */
    public Tones getTone() {
        return tone;
    }

/**
 * Metodo que muda o valor de tone da lampada
 * 
 * @param tone Valor do tom de luz a ser colocado na variavel de instancia tone
 */
    public void setTone(Tones tone) {
        this.tone = tone;
    }

    /**
     * Metodo que retorna a dimensao do objeto
     * @return valor da dimensao do objeto
     */
    public int getDimension() {
        return dimension;
    }

    /**
     * Metodo que calcula o consumo da lampada em funcao do consumo base e o seu tom de luz, tendo em conta se o dispositivo se encontra ligado ou nao
     * @return valor do consumo calculado
     */
    public double comsumption(){
        return (this.isOn() ? 1 : 0) * (this.getConsume() + tone.getConsume());
    }

    /**
     * Metodo que copia o objeto atual
     * @return objeto copia do objeto atual
     */
    public SmartBulb clone(){
        return new SmartBulb(this);
    }

    /**
     * Metodo que calcula a string que representa o estado do objeto
     * @return string que representa o estado do objeto
     */
    public String toString() {
        return "SmartBulb{" + "id=" + this.getId() + ", on=" + this.isOn() + ", tone=" + tone + ", dimension=" + dimension + ", consume=" + this.getConsume() + '}';
    }

    /**
     * Metodo que compara dois objetos
     * @param o objeto a ser comparado
     * @return true se os objetos forem iguais ou false caso contrario
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;

        SmartBulb smartBulb = (SmartBulb) o;

        if (getDimension() != smartBulb.getDimension()) return false;
        return getTone() == smartBulb.getTone();
    }

    /**
     * Metodo que calcula o valor de hash do objeto
     * @return codigo hash do objeto
     */
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + getTone().hashCode();
        result = 31 * result + getDimension();
        return result;
    }

}
