package smart_houses.smart_devices;

public class SmartCamera extends SmartDevice {
    // width da camera
    private int resolutionX;
    // height da camera
    private int resolutionY;
    // dimensao dos ficheiros gerados pela camera
    private int fileDim;

    //Construtor por omissao
    public SmartCamera(){
        super();
        this.resolutionX = 0;
        this.resolutionY = 0;
        this.fileDim = 0;
    }

    /**
     * Construtor parametrizado
     * @param ligado estado do dispositivo
     * @param consumo consumo base do dispositivo
     * @param resolutionX width do dispositivo
     * @param resolutionY height do dispositivo
     * @param fileDim dimensao dos ficheiros gerados pela camera
     */
    public SmartCamera(boolean ligado, double consumo, int resolutionX, int resolutionY, int fileDim){
        super(ligado, consumo);
        this.resolutionX = resolutionX;
        this.resolutionY = resolutionY;
        this.fileDim = fileDim;
    }

    /**
     * Contrutor de copia
     * @param camera objeto a ser copiado
     */
    public SmartCamera(SmartCamera camera){
        super(camera);
        this.resolutionX = camera.getResolutionX();
        this.resolutionY = camera.getResolutionY();
        this.fileDim = camera.getFileDim();
    }

    /**
     * Metodo getter para a width da camera
     * @return valor do width da camera
     */
    public int getResolutionX() {
        return resolutionX;
    }

    /**
     * Metodo setter para a width da camera
     * @param resolutionX valor da width da camera
     */
    public void setResolutionX(int resolutionX) {
        this.resolutionX = resolutionX;
    }

    /**
     * Metodo getter para a height da camera
     * @return valor do height da camera
     */
    public int getResolutionY() {
        return resolutionY;
    }

    /**
     * Metodo setter para a height da camera
     * @param resolutionY valor da height da camera
     */
    public void setResolutionY(int resolutionY) {
        this.resolutionY = resolutionY;
    }

    /**
     * Metodo getter para o tamanho dos ficheiros gerados pela camera
     * @return fileDim do tamanho dos ficheiros
     */
    public int getFileDim() {
        return fileDim;
    }

    /**
     * Metodo setter para o tamanho dos ficheiros
     * @param fileDim valor novo para o tamanho dos ficheiros
     */
    public void setFileDim(int fileDim) {
        this.fileDim = fileDim;
    }

    /**
     * Metodo que calcula string que representa o objeto
     * @return string que representa o objeto
     */
    public String toString() {
        return "SmartCamera{" +
                "id=" + this.getId() +
                ", on=" + this.isOn() +
                ", resolutionX=" + resolutionX +
                ", resolutionY=" + resolutionY +
                ", fileDim=" + fileDim +
                '}';
    }

    /**
     * Metodo que compara o objeto atual com outro
     * @param o objeto a ser comparado
     * @return true caso os objetos sejam iguais, false caso contrario
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;

        SmartCamera that = (SmartCamera) o;

        if (getResolutionX() != that.getResolutionX()) return false;
        if (getResolutionY() != that.getResolutionY()) return false;
        return getFileDim() == that.getFileDim();
    }

    /**
     * Codigo hash do objeto
     * @return valor do codigo hash do objeto
     */
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + getResolutionX();
        result = 31 * result + getResolutionY();
        result = 31 * result + getFileDim();
        return result;
    }

    /**
     * Metodo que cria uma copia do objeto
     * @return copia do objeto
     */
    public SmartCamera clone(){
        return new SmartCamera(this);
    }

    /**
     * Metodo que calcula o consumo da camera em funcao da resolucao e do tamanho do ficheiro que este gera
     * @return valor do consumo do dispositivo
     */
    public double comsumption(){
        return this.getConsume()  + (this.resolutionX * 0.001 * this.resolutionY * 0.001 + this.fileDim * 0.001);
    }
}
