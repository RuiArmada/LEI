package smart_houses.smart_devices;

import java.io.Serializable;

public abstract class SmartDevice implements Serializable {

    // Variavel de classe para gerar um codigo unico para cada SmartDevice
    public static int next_id = 1;

    // estado do dispositivo(ligado/desligado)
    private boolean on;
    // codigo do dispositivo
    private final int id;
    // consumo base do dispositivo
    private double consume;

// Construtor por omissao dos SmartDevice
    public SmartDevice(){
        this.on = false;
        this.id = SmartDevice.next_id++;
        this.consume = 0;
    }

    /**
     * Construtor parametrizado
     * @param on estado do dispositivo
     * @param consume consumo base do dispositivo
     */
    public SmartDevice(boolean on, double consume){
        this.id = SmartDevice.next_id++;
        this.on = on;
        this.consume = consume;
    }

    /**
     * Construtor de copia
     * @param device dispositivo a ser copiado
     */
    public SmartDevice(SmartDevice device){
        this.on = device.isOn();
        this.id = device.getId();
        this.consume = device.getConsume();
    }

    /**
     * Metodo que testa se o dispositivo esta ligado
     * @return true caso esteja ligado ou false caso contrario
     */
    public boolean isOn() {
        return on;
    }

    /**
     * Metodo que liga ou desliga um dispositivo
     * @param on valor a colocar na variavel de instancia on
     */
    public void setOn(boolean on) {
        this.on = on;
    }

    /**
     * Metodo que retorna o id do dispositivo
     * @return o valor do id do dispositivo
     */
    public int getId() {
        return id;
    }

    /**
     * Metodo getter para o consumo
     * @return valor do consumo
     */
    public double getConsume() {
        return this.consume;
    }

    /**
     * Metodo setter do consumo
     * @param consume valor do consumo a colocar no dispositivo
     */
    public void setConsume(double consume) {
        this.consume = consume;
    }

    /**
     * Retorna uma string que representa o objeto
     * 
     * @return String que representa o objeto
     */
    public abstract String toString();

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SmartDevice that = (SmartDevice) o;

        if (isOn() != that.isOn()) return false;
        if (Double.compare(that.getConsume(), getConsume()) != 0) return false;
        return getId() == that.getId();
    }

    /**
     * Metodo que retorna o código de hash do objeto
     * @return valor do codigo de hash
     */
    public int hashCode() {
        int result;
        long temp;
        result = (isOn() ? 1 : 0);
        result = 31 * result + getId();
        temp = Double.doubleToLongBits(getConsume());
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    /**
     * metodo que cria uma copia do objeto atual
     * 
     * @return Objeto copia
     */
    public abstract SmartDevice clone();

    /**
     * Metodo que retorna o consumo do dispositivo
     * 
     * @return The comsumption method is being returned.
     */
    public abstract double comsumption();


}
