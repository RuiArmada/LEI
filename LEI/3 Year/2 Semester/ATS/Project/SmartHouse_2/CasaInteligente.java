import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Classe que implementa uma Casa Inteligente, compreendendo as devidas
 * declarações de variáveis de instância, a codificação dos construtores, a
 * codificação dos métodos de acesso às variáveis de instância e a codificação
 * de outros métodos que são relevantes para definir o comportamento da Casa
 * Inteligente. Além disso, faz a gestão dos SmartDevices e as divisões que existem     
 * 
 * @author grupoPOO 47
 * @version 20220513
 */
public class CasaInteligente 
{
    private String ownerName;                       // Nome do Proprietário
    private int ownerNIF;                           // NIF do proprietário
    private Map<Integer, SmartDevice> devices;      // Identificador, SmartDevice
    private Map<String, List<Integer>> locations;   // Espaço, Lista de Códigos dos Dispositivos
    private String supplier;                        // Fornecedor de Energia

    /**
     * Construtores da classe Casa Inteligente
     */

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public CasaInteligente()
    {
        this.devices = new HashMap<Integer, SmartDevice>();
        this.locations = new HashMap<String, List<Integer>>();
        this.ownerName = "";
        this.ownerNIF = 0;
        this.supplier = "";
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param ownerName nome do dono da casa
     * @param ownerNIF nif do dono da casa
     * @param supplier nome do forncedor da casa
     */
    public CasaInteligente(String ownerName, int ownerNIF, String supplier) 
    {
        this.devices = new HashMap<Integer, SmartDevice> ();
        this.locations = new HashMap<String, List<Integer>>();
        this.ownerName = ownerName;
        if(ownerNIF < 0) this.ownerNIF = 0;
        else this.ownerNIF = ownerNIF;
        this.supplier = supplier;
    }

    /**
     * Construtor de parametrização das variáveis de instância
     * 
     * @param ownerName nome do dono da casa
     * @param ownerNIF nif do dono da casa
     */
    public CasaInteligente(String ownerName, int ownerNIF) 
    {
        this.devices = new HashMap<Integer, SmartDevice> ();
        this.locations = new HashMap<String, List<Integer>>();
        this.ownerName = ownerName;
        this.ownerNIF = ownerNIF;
    }

    /**
     * Construtor de cópia das variáveis de instância
     * 
     * @param o var. que guarda uma casa inteligente
     */
    public CasaInteligente(CasaInteligente o) 
    {
        this.devices = new HashMap<Integer, SmartDevice> ();
        this.locations = new HashMap<String, List<Integer>>();
        this.ownerName = o.getOwnerName();
        this.ownerNIF = o.getOwnerNIF();
        this.supplier = o.getSupplier();
        this.devices = o.getDevices();
        this.locations = o.getLocations();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o nome do proprietário da Casa Inteligente
     * 
     * @return nome do proprietário
     */
    public String getOwnerName() 
    {
        return this.ownerName;
    }

    /**
     * Devolve o NIF do proprietário da Casa Inteligente
     * 
     * @return NIF do proprietário
     */
    public int getOwnerNIF() 
    {
        return this.ownerNIF;
    }

    /**
     * Devolve a lista de dispositivos presentes na Casa Inteligente
     * 
     * @return lista de dispositivos
     */
    public Map<Integer, SmartDevice> getDevices() 
    {
        return this.devices;
    }

    /**
     * Devolve a lista de divisões da Casa Inteligente
     * 
     * @return lista de dispositivos
     */
    public Map<String, List<Integer>> getLocations() 
    {
        return this.locations;
    }

    /**
     * Devolve o fornecedor de energia da Casa Inteligente
     * 
     * @return fornecedor de energia
     */
    public String getSupplier() 
    {
        return this.supplier;
    }
    
    /**
     * Atualiza o nome do proprietário
     * 
     * @param ownerName novo nome do proprietário
     */
    public void setOwnerName(String ownerName) 
    {
        this.ownerName = ownerName;
    }

    /**
     * Atualiza o NIF do proprietário
     * 
     * @param ownerNIF novo NIF do proprietário
     */
    public void setOwnerNIF(int ownerNIF) 
    {
        this.ownerNIF = ownerNIF;
    }

    /**
     * Atualiza a lista de dispositivos da Casa Inteligente
     * 
     * @param devices nova lista de dispositivos
     */
    public void setDevices(Map<Integer, SmartDevice> devices) 
    {
        this.devices = devices;
    }

    /**
     * Atualiza a lista de dispositivos presentes na Casa Inteligente
     * 
     * @param locations nova lista de localizações
     */
    public void setLocations(Map<String, List<Integer>> locations) 
    {
        this.locations = locations;
    }

    /**
     * Atualiza o fornecedor de energia da Casa Inteligente
     * 
     * @param supplier novo fornecedor de energia
     */
    public void setSupplier(String supplier) 
    {
        this.supplier = supplier;
    }

    /**
     * Método que permite saber se o dispositivo existe na Casa Inteligente
     * 
     * @param id ID do dispositivo
     * @return retorna verdadeiro ou falso, se existir ou não o dispositivo
     */
    public boolean existsDevice(Integer id) 
    {
        return this.devices.containsKey(id);
    }

    /**
     * Método que permite adicionar um dispositivo numa determinada divisão da Casa Inteligente
     * 
     * @param id ID do dispositivo
     * @param location divisão da casa onde será adicionado o dispositivo
     * @param smartDevice dispositivo a ser adicionado
     */
    public void addSmartDevice(int id, String location, SmartDevice smartDevice) 
    {
        this.devices.put(id, smartDevice);
        if (this.locations.containsKey(location)) 
        {
            this.locations.get(location).add(id);
        } 
        else 
        {
            this.locations.put(location, new ArrayList<Integer>());
            this.locations.get(location).add(id);
        }
    }
    
    /**
     * Método que retorna o dispositivo referente ao ID dado
     * 
     * @param id ID do dispositivo
     * @return dispositivo correspondente
     */
    public SmartDevice getDevice(int id)
    {
        return this.devices.get(id);
    }
    
    /**
     * Método utilizado para ligar todos os dispositivos da Casa Inteligente
     * 
     * @param bool verdadeiro ou falso
     */
    public void setAllOn(boolean bool) 
    {
        for (SmartDevice smartDevice : this.devices.values()) 
        {
            smartDevice.setOn(bool);
        }
    }

    /**
     * Método que liga e desliga o dispositivo
     * 
     * @param id ID do dispositivo
     * @param on estado do dispositivo (verdadeiro-on/falso-off)
     */
    public void setSmartDeviceOnOff(int id, boolean on) 
    {
        this.devices.get(id).setOn(on);
    }

    /**
     * Método que liga todos os dispositivos de uma determinada divisão da Casa
     * 
     * @param location divisão da Casa
     * @param bool verdadeiro ou falso
     */
    public void setAllOnLocation(String location, boolean bool)
    {
        List<Integer> local = this.locations.get(location);
        for (Integer id : local) 
        {
            this.devices.get(id).setOn(bool);
        }
    }

    /**
     *  Método para adicionar uma divisão à Casa Inteligente
     * 
     * @param room divisão a adicionar
     */
    public void addRoom(String room) 
    {
        List<Integer> location = new ArrayList<>();
        this.locations.put(room,location);
    }
    
    /**
     * Método que permite saber se existe uma determinada divisão na Casa Inteligente
     * 
     * @param room divisão da casa
     * @return verdadeiro ou falso
     */
    public boolean hasRoom(String room) 
    {
        return this.locations.containsKey(room);
    }
    
    /**
     * Métod que permite adicionar um dispositivo a uma divisão da Casa Inteligente
     * 
     * @param room divisão onde se quer adicionar o dispositivo
     * @param id ID do dispositivo
     */
    public void addToRoom (String room, Integer id) 
    {
        List<Integer> local = this.locations.get(room);
        local.add(id);
    }
    
    /**
     * Método que permite saber se a divisão tem o dispositivo em questão
     * 
     * @param room divisão da Casa a pesquisar
     * @param id ID do dispositivo a procurar
     * @return verdadeiro ou falso
     */
    public boolean roomHasDevice (String room, Integer id) 
    {
        List<Integer> local = this.locations.get(room);
        return local.contains(id);
    }

    /**
     * Método que devolve o consumo total de todos os dispositivos da Casa
     * 
     * @return valor do consumo total dos dispositivos da casa
     */
    public double getTotalConsumption() 
    {
        double total = 0;
        for (SmartDevice r : this.devices.values()) 
        {
            total += r.dailyConsumption();
        }
        return total;
    }
    
    /**
     * Método equals que todos os objetos possuem
     * 
     * @param o o objeto é comparado com o recetor da mensagem
     * @return verdadeiro ou falso
     */
    public boolean equals(Object o) 
    {
        if (this == o) 
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }
        CasaInteligente c = (CasaInteligente) o;
        return this.ownerNIF == c.getOwnerNIF() &&
                this.ownerName.equals(c.getOwnerName()) &&
                this.supplier.equals(c.getSupplier()) &&
                this.devices.equals(c.getDevices()) &&
                this.locations.equals(c.getLocations());
    }

    /**
     * Método que devolve a representação em String da Casa Inteligente
     * 
     * @return String com as variáveis de instância da Casa Inteligente
     */
    public String toString() 
    {
        return "[Casa Inteligente] : Dono = " + ownerName + " | NIF = " + ownerNIF + " | Fornecedor = " + supplier + " ; ";
        
    }

    /**
     * Método que faz uma cópia do objeto recetor da mensagem. 
     * Para tal invoca o construtor de cópia
     * 
     * @return objeto clone do objeto que recebe a mensagem
     */
    public CasaInteligente clone() 
    {
        return new CasaInteligente(this);
    }
}
