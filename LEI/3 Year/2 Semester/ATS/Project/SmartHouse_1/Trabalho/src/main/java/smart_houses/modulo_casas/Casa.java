package smart_houses.modulo_casas;

import smart_houses.Fatura;
import smart_houses.exceptions.*;
import smart_houses.smart_devices.SmartBulb;
import smart_houses.smart_devices.SmartCamera;
import smart_houses.smart_devices.SmartDevice;
import smart_houses.smart_devices.SmartSpeaker;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class Casa implements Serializable {

     // nome do proprietário da casa
    private final String nome;

     // nif do proprietário da casa
    private final String nif;

     // coleçao de devices, Map de codigo de device para o proprio device
    private Map<Integer, SmartDevice> devices;

    // Map de nome da divisão para a coleção de codigos dos devices que se encontram na divisão
    private Map<String, Set<Integer>> rooms;

     // Lista de faturas da casa
    private final List<Fatura> faturas;

     // Nome do fornecedor da casa
    private String fornecedor;

     // Construtor por omissão de parametros
    public Casa(){
        this.nome = "";
        this.nif = "";
        this.devices = new HashMap<>();
        this.rooms = new HashMap<>();
        this.fornecedor = "";
        this.faturas = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param nome nome do proprietário
     * @param nif nif do proprietário
     * @param fornecedor nome do fornecedor da casa
     */
    public Casa(String nome, String nif, String fornecedor){
        this.nome = nome;
        this.nif = nif;
        this.devices = new HashMap<>();
        this.rooms = new HashMap<>();
        this.fornecedor = fornecedor;
        this.faturas = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param nome nome do proprietário
     * @param nif nif do proprietário
     * @param rooms colecao de divisões a incluir na nova casa
     * @param fornecedor fornecedor da casa
     */
    public Casa(String nome, String nif, Set<String> rooms, String fornecedor){
        this.nome = nome;
        this.nif = nif;
        this.devices = new HashMap<>();
        this.setRooms(rooms);
        this.fornecedor = fornecedor;
        this.faturas = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param nome nome do proprietário
     * @param nif nif do proprietário
     * @param rooms colecao das divisoes a incluir, acompanhada com os devices que vão estar nela
     * @param fornecedor nome do forneceodr da casa
     */
    public Casa(String nome, String nif, Map<String, Set<Integer>> rooms, String fornecedor){
        this.nome = nome;
        this.nif = nif;
        this.devices = new HashMap<>();
        this.setRooms(rooms);
        this.fornecedor = fornecedor;
        this.faturas = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param nome nome do proprietário
     * @param nif nif do proprietário
     * @param devices colecao dos devices a colocar na casa
     * @param rooms colecao de divisoes a colocar na casa
     * @param fornecedor nome do fornecedor da casa
     */
    public Casa(String nome, String nif, Map<Integer, SmartDevice> devices, Map<String, Set<Integer>> rooms, String fornecedor){
        this.nome = nome;
        this.nif = nif;
        this.setDevices(devices);
        this.setRooms(rooms);
        this.fornecedor = fornecedor;
        this.faturas = new ArrayList<>();
    }

    /**
     * Construtor de Cópia
     * @param casa casa a copiar
     */
    public Casa(Casa casa){
        this.nome = casa.getNome();
        this.nif = casa.getNif();
        this.devices = casa.getMapDevices();
        this.rooms = casa.getRooms();
        this.fornecedor = casa.getFornecedor();
        this.faturas = casa.getFaturas();
    }

    /**
     * Método que adiciona uma fatura na coleção da casa
     * @param fatura fatura a adicionar na casa
     */
    public void adicionaFatura(Fatura fatura){
        this.faturas.add(fatura.clone());
    }

    /**
     * Método que retorna a cópia das faturas da casa
     * @return cópia das faturas da casa
     */
    public List<Fatura> getFaturas() {
        return this.faturas.stream().map(Fatura::clone).collect(Collectors.toList());
    }

    /**
     * Método getter para obter o nome do fornecedor
     * @return nome do fornecedor
     */
    public String getFornecedor() {
        return fornecedor;
    }

    /**
     * Método setter para mudar para um novo fornecedor na casa
     * @param fornecedor nome do fornecedor novo
     */
    public void setFornecedor(String fornecedor) {
        this.fornecedor = fornecedor;
    }

    /**
     * Método getter para o nome do proprietário
     * @return nome do proprietário
     */
    public String getNome() {
        return nome;
    }

    /**
     * Método getter do nif do proprietário
     * @return nif do proprietário
     */
    public String getNif() {
        return nif;
    }

    /**
     * @param o Objeto para testar se é igual ao objeto atual
     * @return valor se o objeto é igual ou nao ao atual
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Casa casa = (Casa) o;

        return this.nome.equals(casa.getNome()) && this.nif.equals(casa.getNif()) && this.devices.equals(casa.getMapDevices()) &&
                this.rooms.equals(casa.getRooms()) && this.faturas.equals(casa.getFaturas()) && this.fornecedor.equals(casa.getFornecedor());
    }

    /**
     * @return retorna o codigo de hash para o objeto
     */
    public int hashCode() {
        int result = 7;
        result = 31 * result + getNome().hashCode();
        result = 31 * result + getNif().hashCode();
        result = 31 * result + devices.hashCode();
        result = 31 * result + getRooms().hashCode();
        result = 31 * result + getFaturas().hashCode();
        result = 31 * result + getFornecedor().hashCode();
        return result;
    }

    /**
     * Metodo que calcula a String com o estado do objeto
     * @return String que representa o estado do objeto do tipo casa
     */
    public String toString() {
        return "Casa{" +
                "nome='" + nome + '\'' +
                ", nif='" + nif + '\'' +
                ", devices=" + devices +
                ", rooms=" + rooms +
                ", faturas=" + faturas +
                ", fornecedor='" + fornecedor + '\'' +
                '}';
    }

    /**
     * Método que retorna um Map com as cópias dos devices
     * @return colecao de devices como Map
     */
    public Map<Integer, SmartDevice> getMapDevices() {
        return this.devices.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().clone()));
    }


    /**
     * Método que retorna a cópia dos devices como uma Lista
     * @return lista de devices
     */
    public List<SmartDevice> getListDevices(){
        return this.devices.values().stream().map(SmartDevice::clone).collect(Collectors.toList());
    }

    /**
     * Método que retorna os nomes das divisões da casa
     * @return lista com os nomes das divisões
     */
    public List<String> getListRooms(){
        return new ArrayList<>(this.rooms.keySet());
    }

    /**
     * Setter para devices da casa
     * @param devices devices a colocar na casa
     */
    public void setDevices(Map<Integer, SmartDevice> devices) {
        this.devices = devices.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().clone()));
    }

    /**
     * Getter para as divisões da casa
     * @return colecao relativa a rooms
     */
    public Map<String, Set<Integer>> getRooms() {
        return this.rooms.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> new TreeSet<>(e.getValue())));
    }

    /**
     * Setter para divisões da casa
     * @param rooms divisões a colocar na casa
     */
    public void setRooms(Map<String, Set<Integer>> rooms) {
        this.rooms = rooms.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> new TreeSet<>(e.getValue())));
    }

    /**
     * Método que recebe uma colecao com os nomes das divisões e adiciona na casa
     * @param rooms divisoes a colocar na casa
     */
    public void setRooms(Set<String> rooms) {
        this.rooms = rooms.stream().collect(Collectors.toMap(r -> r, r -> new TreeSet<>()));
    }

    /**
     * Método que coloca todos os devices de uma divisão ligados ou desligados
     * @param room divisão onde desejo alterar os devices
     * @param on ligar ou desligar o dispositivo
     * @throws RoomInexistenteException não existe a divisão passada
     */
    public void setAllDevicesStateRoom(String room, boolean on) throws RoomInexistenteException {
      Set<Integer> devices = this.rooms.get(room);
      if(devices == null) throw new RoomInexistenteException("Nao existe a divisao : " + room + " nesta casa");
        this.rooms.get(room).forEach(device -> this.devices.get(device).setOn(on));
    }

    /**
     * Método que retorna uma copia da casa atual
     * @return Copia da casa
     */
    public Casa clone(){
        return new Casa(this);
    }

    /**
     * Método que liga ou desliga todos os devices da casa
     * @param state ligar ou desligar os dispositivos
     */
    public void setAllDevicesState(boolean state){
        this.devices.values().forEach(device -> device.setOn(state));
    }

    /**
     * Método que liga ou desliga um determinado dispositivo
     * @param id id do dispositivo a alterar
     * @param state liga ou desliga o dispositivo
     * @throws DeviceInexistenteException nao existe o dispositivo
     */
    public void setDeviceState(int id, boolean state) throws DeviceInexistenteException {
        SmartDevice device = this.devices.get(id);
        if(device == null) throw new DeviceInexistenteException("Não existe o device de id " + id + "nesta casa");
        this.devices.get(id).setOn(state);
    }

    /**
     * @param device Novo dispositivo
     * @throws AlreadyExistDeviceException Já existe o dispositivo
     */
    public void addDevice(SmartDevice device) throws AlreadyExistDeviceException {
        if(this.devices.containsKey(device.getId())) throw new AlreadyExistDeviceException("Já existe um device com o código " + device.getId());
        this.devices.put(device.getId(), device.clone());
    }

    /**
     * Método que remove um dispositivo da casa
     * @param id codigo do dispositivo a remover
     * @throws DeviceInexistenteException caso nao exista o device
     */
    public void removeDevice(int id) throws DeviceInexistenteException {
        if(!this.devices.containsKey(id)) throw new DeviceInexistenteException("Nao existe dispositivo de id " + id);
        this.devices.remove(id);
        Iterator<Set<Integer>> d = this.rooms.values().iterator();
        boolean found = false;
        while(d.hasNext() && !found){
            Set<Integer> ds = d.next();
            found = ds.remove(id);
        }
    }

    /**
     * Método que adiciona uma divisão à casa
     * @param room nome da divisão a adicionar na casa
     * @throws RoomAlreadyExistsException Caso já exista uma divisão com o nome inserido
     */
    public void addRoom(String room) throws RoomAlreadyExistsException{
        if(this.rooms.containsKey(room)) throw new RoomAlreadyExistsException("A divisao: " + room + " já existe nesta casa");
        this.rooms.put(room, new TreeSet<>());
    }

    /**
     * Método que remove uma divisão da casa
     * @param room divisão que se pretende remover
     */
    public void removeRoom(String room){
        this.rooms.remove(room);
    }

    public void addDeviceOnRoom(String room, int device) throws RoomInexistenteException, DeviceInexistenteException {
        if(!this.rooms.containsKey(room)) throw new RoomInexistenteException("Esta room nao existe na casa");
        if(!this.devices.containsKey(device)) throw new DeviceInexistenteException("Nao existe nenhum device com o id de " + device);
        this.rooms.get(room).add(device);
    }

    /**
     * Método que remove o dispositivo da divisão onde se encontra
     * @param device dispositivo a remover da divisão
     * @throws DeviceInexistenteException caso o dispositivo nao exista na casa
     */
    public void removeDeviceOnRoom(int device) throws DeviceInexistenteException {
        if(!this.devices.containsKey(device)) throw new DeviceInexistenteException("Não existe dispositivo com código " + device);
        String room = divisaoDeDispositivo(device);
        if(room != null){
            this.rooms.get(room).remove(device);
        }
    }

    /**
     * Método que calcula o consumo de todos os dispositivos da casa
     * @return consumo de todos os dispositivos
     */
    public double consumoDispositivos(){
        return this.devices.values().stream().mapToDouble(SmartDevice::comsumption).sum();
    }

    /**
     * Método que calcula o consumo da casa no ultimo periodo do programa, ou seja na ultima fatura
     * @return consumo da casa no ultimo periodo(ultima fatura gerada)
     */
    public double consumoPeriodo(){
        if(this.faturas.size() == 0) return 0;
        else return this.faturas.get(this.faturas.size() - 1).getCusto();
    }

    /**
     * Método qeu calcula o periodo de uma casa entre duas datas
     * @param inicio data inicial do periodo
     * @param fim data final do periodo
     * @return consumo no periodo indicado
     */
    public double consumoPeriodo(LocalDate inicio, LocalDate fim){
        return this.faturas.stream().filter(f -> (f.getInicioPeriodo().isEqual(inicio) || f.getInicioPeriodo().isAfter(inicio)) && (f.getFimPeriodo().isEqual(fim) || f.getFimPeriodo().isBefore(fim))).mapToDouble(Fatura::getCusto).sum();
    }

    /**
     * Método que calcula a divisão onde um device existe
     * @param device device que se pretende procurar
     * @return divisão onde se encontra o dispositivo
     */
    public String divisaoDeDispositivo (int device){
        String room = null;
        Iterator<Map.Entry<String, Set<Integer>>> iter = this.rooms.entrySet().iterator();

        while(iter.hasNext() && room == null){
            Map.Entry<String, Set<Integer>> div = iter.next();
            Iterator<Integer> iter1 = div.getValue().iterator();
            while(iter1.hasNext() && room == null){
                int dev = iter1.next();
                if(dev == device) room = div.getKey();
            }
        }

        return room;
    }

    /**
     * Método que muda o dispositivo para uma divisão, removendo-o das divisões onde se encotrava
     * @param room divisão onde se pretende colocar o device
     * @param device device que se pretende mudar de divisão
     * @throws DeviceInexistenteException caso nao exista o dispositvo com o codigo recebido como parametro
     * @throws RoomInexistenteException caso nao exista a divisão para onde se quer colocar o dispositivo
     */
    public void mudaDeviceDeRoom(String room, int device) throws DeviceInexistenteException, RoomInexistenteException {
        if(!this.devices.containsKey(device)) throw new DeviceInexistenteException("Não existe o device de código : " + device);
        if(!this.rooms.containsKey(room)) throw new RoomInexistenteException("Não existe a divisao " + room + " nesta casa");

        String divisao = this.divisaoDeDispositivo(device);
        if(divisao != null){
            this.rooms.get(divisao).remove(device);
        }
        this.rooms.get(room).add(device);
    }

    /**
     * Método que dado um codigo do dispositivo e o comportamento que se pretende realizar aplica esse mesmo comportamento no device
     * @param room1 1ª room que se pretende juntar
     * @param room2 2ª room que se pretende juntar
     * @param nova divisão que se pretende criar a partir da junçao das outras duas
     * @throws RoomAlreadyExistsException Caso a divisão que se vai criar já existe
     */
    public void juntaRooms(String room1, String room2, String nova) throws RoomAlreadyExistsException {
        if(this.rooms.containsKey(nova)) throw new RoomAlreadyExistsException("Esta room ja existe na casa");

        Set<Integer> devices1 = this.rooms.get(room1);
        Set<Integer> devices2 = this.rooms.get(room2);

        this.rooms.put(nova, new TreeSet<>());
        if(devices1 != null) devices1.forEach(d -> this.rooms.get(nova).add(d));
        if(devices2 != null) devices2.forEach(d -> this.rooms.get(nova).add(d));
        this.rooms.remove(room1);
        this.rooms.remove(room2);

    }

    /**
     * Método que dado um codigo do dispositivo e o comportamento que se pretende realizar aplica esse mesmo comportamento no device
     * @param id id do SmartBulb que se pretende alterar
     * @param mapperBulb comportamente que se pretende realizar no SmartBulb
     * @throws DeviceInexistenteException caso o dispositivo não exista na casa
     * @throws TipoDeviceErradoException caso o dispositivo que se pretenda alterar não seja um SmartBulb
     */
    public void alteraInfoBulb(int id, Consumer<SmartBulb> mapperBulb) throws DeviceInexistenteException, TipoDeviceErradoException {
      SmartDevice device = this.devices.get(id);
      if(device == null) throw new DeviceInexistenteException("Não existe um device com id de " + id + " na casa de nif " + this.nif);

      if(!(device instanceof SmartBulb)) throw new TipoDeviceErradoException("O tipo de device procurado não é do tipo SmartBulb");

      mapperBulb.accept((SmartBulb) device);

    }

    /**
     * Método que dado um codigo do dispositivo e o comportamento que se pretende realizar aplica esse mesmo comportamento no device
     * @param id id do SmartCamera que se pretende alterar
     * @param mapperCamera comportamente que se pretende realizar no SmartCamera
     * @throws DeviceInexistenteException caso o dispositivo não exista na casa
     * @throws TipoDeviceErradoException caso o dispositivo que se pretenda alterar não seja um SmartCamera
     */
    public void alteraInfoCamera(int id, Consumer<SmartCamera> mapperCamera) throws DeviceInexistenteException, TipoDeviceErradoException {
      SmartDevice device = this.devices.get(id);
      if(device == null) throw new DeviceInexistenteException("Não existe um device com id de " + id + " na casa de nif " + this.nif);

      if(!(device instanceof SmartCamera)) throw new TipoDeviceErradoException("O tipo de device procurado não é do tipo SmartCamera");

      mapperCamera.accept((SmartCamera) device);

    }

    /**
     * Método que dado um codigo do dispositivo e o comportamento que se pretende realizar aplica esse mesmo comportamento no device
     * @param id id do SmartSpeaker que se pretende alterar
     * @param mapperSpeaker comportamente que se pretende realizar no SmartSpeaker
     * @throws DeviceInexistenteException caso o dispositivo não exista na casa
     * @throws TipoDeviceErradoException caso o dispositivo que se pretenda alterar não seja um SmartSpeaker
     */
    public void alteraInfoSpeaker(int id, Consumer<SmartSpeaker> mapperSpeaker) throws DeviceInexistenteException, TipoDeviceErradoException {
      SmartDevice device = this.devices.get(id);
      if(device == null) throw new DeviceInexistenteException("Não existe um device com id de " + id + " na casa de nif " + this.nif);

      if(!(device instanceof SmartSpeaker)) throw new TipoDeviceErradoException("O tipo de device procurado não é do tipo SmartSpeaker");

      mapperSpeaker.accept((SmartSpeaker) device);

    }

    /**
     * Método que dado um codigo retorna a cópia do device
     * @param id codigo do device
     * @return cópia do device
     * @throws DeviceInexistenteException caso o device nao exista na casa
     */
    public SmartDevice getDevice(int id) throws DeviceInexistenteException {
      if(!this.devices.containsKey(id)) throw new DeviceInexistenteException("Não existe o device de id " + id + " na casa de nif " + this.nif);
      return this.devices.get(id);
    }

    /**
     * metodo que calcula o consumo total que uma casa gastou no programa no total
     * @return consumo da casa
     */
    public double consumoTotal(){
        return this.faturas.stream().mapToDouble(Fatura::getCusto).sum();
    }

}
