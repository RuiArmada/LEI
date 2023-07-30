package smart_houses.smart_devices;


public class SmartSpeaker extends SmartDevice {

    // Valor maximo para o volume dos SmartSpeakers
    private static final int MAX = 100;

    // Volume do speaker
    private int volume;
    // Estacao de radio do speaker
    private String radioStation;
    // Marca do speaker
    private String brand;

    /**
     * Contrutor por omissao
     */
    public SmartSpeaker(){
        super();
        this.volume=0;
        this.radioStation="n/a";
        this.brand="n/a";
    }

    /**
     * Construtor parametrizado
     * @param on estado do dispositivo
     * @param consume consumo do dispositivo
     * @param volume volume do dispositivo
     * @param radioStation estacao de radio
     * @param brand marca do dispositivo
     */
    public SmartSpeaker(boolean on, double consume, int volume, String radioStation, String brand){
        super(on, consume);
        this.volume=volume;
        this.radioStation= radioStation;
        this.brand=brand;
    }

    /**
     * Contrutor de copia
     * @param device dispositivo a ser copiado
     */
    public SmartSpeaker(SmartSpeaker device){
        super(device);
        this.volume= device.getVolume();
        this.radioStation=device.getRadioStation();
        this.brand=device.getBrand();
    }

    /**
     * Metodo getter para o volume
     * @return valor do volume
     */
    public int getVolume() {
        return volume;
    }

    /**
     * Metodo setter para o volume
     * @param volume valor do volume a ser colocado
     */
    public void setVolume(int volume) {
        if(volume > 100) volume = SmartSpeaker.MAX;
        else if(volume < 0) volume = 0;
        this.volume = volume;
    }

    /**
     * Metodo getter da estacao de radio
     * @return valor da estacao de radio
     */
    public String getRadioStation() {
        return radioStation;
    }

    /**
     * metodo setter para a estacao de radio
     * @param radioStation valor da nova estacao de radio a ser colocada
     */
    public void setRadioStation(String radioStation) {
        this.radioStation = radioStation;
    }

    /**
     * Metodo getter da marca do dispositivo
     * @return valor da marca do dispositivo
     */
    public String getBrand() {
        return brand;
    }

    /**
     * Metodo que calcula string que representa o objeto
     * @return string com a representacao do objeto
     */
    public String toString() {
        return "SmartSpeaker{" +
                "id = " + this.getId() +
                ", on = " + this.isOn() +
                "volume=" + volume +
                ", radioStation='" + radioStation + '\'' +
                ", brand='" + brand + '\'' +
                '}';
    }

    /**
     * Metodo que compara dois objetos
     * @param o objeto a ser comparado
     * @return true se forem iguais, false caso contrario
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;

        SmartSpeaker that = (SmartSpeaker) o;

        if (getVolume() != that.getVolume()) return false;
        if (!getRadioStation().equals(that.getRadioStation())) return false;
        return getBrand().equals(that.getBrand());
    }

    /**
     * Metodo que calcula o codigo hash do objeto
     * @return valor do codigo de hash do objeto
     */
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + getVolume();
        result = 31 * result + getRadioStation().hashCode();
        result = 31 * result + getBrand().hashCode();
        return result;
    }

    /**
     * Metodo que cria copia do objeto
     * @return Copia do objeto
     */
    public SmartSpeaker clone(){
        return new SmartSpeaker(this);
    }

    /**
     * Metodo que calcula o consumo do dispositivo em funcao do volume
     * @return valor do consumo diario
     */
    public double comsumption(){
        return (this.isOn() ? 1 : 0) * (this.getConsume() + this.volume * 0.001);
    }
}
