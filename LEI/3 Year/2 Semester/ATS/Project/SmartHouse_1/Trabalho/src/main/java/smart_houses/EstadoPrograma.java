package smart_houses;

import smart_houses.exceptions.*;
import smart_houses.modulo_casas.Casa;
import smart_houses.modulo_fornecedores.Fornecedor;
import smart_houses.smart_devices.SmartBulb;
import smart_houses.smart_devices.SmartCamera;
import smart_houses.smart_devices.SmartDevice;
import smart_houses.smart_devices.SmartSpeaker;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class EstadoPrograma implements Serializable {
    // Map para as casas, nif para casa
    private final Map<String, Casa> casas;
    // map para fornecedores, nome para fornecedor
    private final Map<String, Fornecedor> fornecedores;

    // Lista de pedidos a serem executados apos avancar o tempo
    private final Queue<SerializableConsumer> pedidos;

    // Data atual do programa
    private LocalDate data_atual;

    // Valor que custa um kWh
    public static final double custoEnergia = 0.15;
    // Imposto energetico
    public static final double imposto = 0.06;


    /**
     * Contrutor de omissao
     */
    public EstadoPrograma() {
        this.casas = new TreeMap<>();
        this.fornecedores = new HashMap<>();
        this.data_atual = LocalDate.now();
        this.pedidos = new LinkedList<>();
    }

    /**
     * Construtor de copia
     * @param c objeto a ser copiado
     */
    public EstadoPrograma(EstadoPrograma c) {
        this.casas = c.getCasas();
        this.fornecedores = c.getFornecedores();
        this.data_atual = c.getDataAtual();
        this.pedidos = c.getPedidos();
    }

    /**
     * Metodo getter para os pedidos que irao ser feitos
     * @return fila de espera com os pedidos
     */
    public Queue<SerializableConsumer> getPedidos() {
        return new LinkedList<>(this.pedidos);
    }

    /**
     * Metodo que adiciona um pedido a fila
     * @param pedido pedido a ser adicionado
     */
    public void addPedido(SerializableConsumer pedido){
        this.pedidos.add(pedido);
    }

    /**
     * Metodo que executa todos os pedidos
     */
    private void runAllRequests(){
        while(!this.pedidos.isEmpty()){
            this.pedidos.poll().accept(this);
        }
    }

    /**
     * Metodo getter para a data atual do programa
     * @return valor da data atual do programa
     */
    public LocalDate getDataAtual() {
        return data_atual;
    }

    /**
     * Metodo que calcula todas as faturas emitindo-as para as casas do programa
     * @param fim periodo final, para onde o programa vai ser colocado
     * @throws FornecedorErradoException caso algum fornecedor tenha calculado a fatura de uma casa cujo fornecedor nao era ele
     */
    private void geraFaturas(LocalDate fim) throws FornecedorErradoException{
        for (Casa casa : this.casas.values()) {
            Fatura f = this.fornecedores.get(casa.getFornecedor()).criaFatura(casa.clone(), this.data_atual, fim);
            casa.adicionaFatura(f);
            this.fornecedores.get(casa.getFornecedor()).adicionaFatura(f);
        }
    }

    /**
     * Metodo que devolve todas as faturas de um dado fornecedor
     * @param nome nome do fornecedor
     * @return faturas do fornecedor
     * @throws FornecedorInexistenteException caso o fornecedor nao exista
     */
    public List<Fatura> getFaturasFornecedor(String nome) throws FornecedorInexistenteException {
        if(!this.fornecedores.containsKey(nome)) throw new FornecedorInexistenteException("Nao existe fornecedor: " + nome);
        return this.fornecedores.get(nome).getFaturas().stream().map(Fatura::clone).collect(Collectors.toList());
    }

    /**
     * Metodo que calcula a casa que mais gastou no programa inteiro
     * @return casa que mais gastou
     */
    public Optional<Casa> getCasaMaisGastadora() {

        return this.casas.values().stream().max(Comparator.comparingDouble(c -> {
            List<Fatura> faturas = c.getFaturas();
            return faturas.stream().mapToDouble(Fatura::getCusto).sum();
        })).map(Casa::clone);

    }

    /**
     * Metodo que gera um TopN de consumidores do último periodo de avanco
     * @param N Numero de nifs a serem colocados no ‘top’
     * @return lista de nifs ordenadas por consumo
     */
    public List<Casa> maiorConsumidorPeriodo(int N) {
        Comparator<Casa> comp = Comparator.comparingDouble(Casa::consumoPeriodo);
        return this.casas.values()
                .stream()
                .sorted(comp.reversed())
                .limit(N)
                .map(Casa::clone)
                .collect(Collectors.toList());
    }

    /**
     * Metodo que calcula os maiores consumidores de um dado periodo
     * @param inicio inicio do periodo a ser contemplado
     * @param fim fim do periodo
     * @param N numero de casas a serem consideradas
     * @return lista com o Top N
     */
    public List<Casa> maiorConsumidorPeriodo(LocalDate inicio, LocalDate fim, int N) {
        Comparator<Casa> comp = Comparator.comparingDouble(c -> c.consumoPeriodo(inicio, fim));
        return this.casas.values()
                .stream()
                .sorted(comp.reversed())
                .limit(N)
                .map(Casa::clone)
                .collect(Collectors.toList());
    }

    /**
     * Metodo que compara dois objetos
     * @param o objeto a ser comparado
     * @return true caso sejam iguais, false caso contrario
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        EstadoPrograma that = (EstadoPrograma) o;

        if (!getCasas().equals(that.getCasas())) return false;
        if (!getFornecedores().equals(that.getFornecedores())) return false;
        if (!getPedidos().equals(that.getPedidos())) return false;
        return data_atual.equals(that.data_atual);
    }

    /**
     * Metodo com o codigo de hash do objeto
     * @return valor do codigo de hash do objeto
     */
    public int hashCode() {
        int result = getCasas().hashCode();
        result = 31 * result + getFornecedores().hashCode();
        result = 31 * result + getPedidos().hashCode();
        result = 31 * result + data_atual.hashCode();
        return result;
    }

    /**
     * Metodo que calcula a string que representa o objeto
     * @return ‘string’ com a ‘string’ que representa o objeto
     */
    public String toString() {
        return "EstadoPrograma{" +
                "casas=" + casas +
                ", fornecedores=" + fornecedores +
                ", pedidos=" + pedidos +
                ", data_atual=" + data_atual +
                '}';
    }

    /**
     * Metodo que calcula o fornecedor que mais faturou ate agora
     * @return nome do fornecedor que mais faturou
     */
    public Fornecedor getFornecedorMaiorFaturacao() throws FornecedorInexistenteException {
        Comparator<Map.Entry<String, Fornecedor>> comp = (f1, f2) -> {
            double faturacao1 = f1.getValue().faturacao();
            double faturacao2 = f2.getValue().faturacao();
            return Double.compare(faturacao1, faturacao2);
        };
        return this.fornecedores.entrySet().stream().max(comp).map(Map.Entry::getValue).orElseThrow(() -> new FornecedorInexistenteException("Nao existe fornecedores para ver o maximo"));
    }

    /**
     * @param date data para a qual se pretende avancar a data
     * @throws DataInvalidaException caso a data passada seja invalida
     * @throws FornecedorErradoException caso tenha havido algum erro no calculo das faturas
     */
    public void avancaData(LocalDate date) throws DataInvalidaException, FornecedorErradoException {
        if(date.isBefore(this.data_atual)) throw new DataInvalidaException("Esta data é anterior à atual");
        this.geraFaturas(date);
        this.data_atual = date;
        this.runAllRequests();
    }

    /**
     * Metodo que carrega dados de um ficheiro de objetos
     * @return Novo objeto calculado a partir do ficheiro
     * @throws IOException caso haja erro a ler o ficheiro
     * @throws ClassNotFoundException erro a ler o objeto
     */
    public static EstadoPrograma carregaDados() throws IOException, ClassNotFoundException {
        ObjectInputStream ois = new ObjectInputStream(new FileInputStream("./src/main/resources/data.obj"));
        int next_idDevice = ois.readInt();
        int next_idFatura = ois.readInt();
        EstadoPrograma programa = (EstadoPrograma) ois.readObject();
        Fatura.next_codigoFatura = next_idFatura;
        SmartDevice.next_id = next_idDevice;
        ois.close();
        return programa;
    }

    /**
     * Metodo que retorna a copia das casas
     * @return Map com as cópias das casas
     */
    public Map<String, Casa> getCasas() {
        return this.casas.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().clone()));
    }

    /**
     * Metodo que retorna as copias dos fornecedores
     * @return Map com as cópias dos fornecedores
     */
    public Map<String, Fornecedor> getFornecedores() {
        return this.fornecedores.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().clone()));
    }

    /**
     * Metodo que adiciona uma casa ao programa
     * @param c casa a ser adicionada
     * @throws ExisteCasaException caso ja haja uma casa com o nif inserido
     * @throws FornecedorInexistenteException caso nao exista o fornecedor da casa
     */
    public void adicionaCasa(Casa c) throws ExisteCasaException, FornecedorInexistenteException{
        if (this.casas.containsKey(c.getNif())) throw new ExisteCasaException("Esta casa tem um nif que ja existe");
        if (!this.fornecedores.containsKey(c.getFornecedor())) throw new FornecedorInexistenteException("Nao existe este fornecedores");
        this.casas.put(c.getNif(), c.clone());
    }

    /**
     * Metodo que retorna a cópia do objeto
     * @return copia do objeto
     */
    public EstadoPrograma clone() {
        return new EstadoPrograma(this);
    }

    /**
     * Metodo que guarda o objeto num ficheiro de objetos
     */
    public void guardaDados() throws IOException {
            FileOutputStream file = new FileOutputStream("./src/main/resources/data.obj");
            ObjectOutputStream oos = new ObjectOutputStream(file);
            oos.writeInt(SmartDevice.next_id);
            oos.writeInt(Fatura.next_codigoFatura);
            oos.writeObject(this);
            oos.close();
    }

    /**
     * Metodo que retorna a lista de divisoes de uma casa
     * @param nif nif da casa na qual se pretende obter as divisoes
     * @return lista das divisoes da casa
     * @throws CasaInexistenteException caso a casa nao exista no programa
     */
    public List<String> getRoomsHouse(String nif) throws CasaInexistenteException {
      Casa casa = this.casas.get(nif);
      if(casa == null) throw new CasaInexistenteException("Esta casa com nif : " + nif);
        return casa.getListRooms();
    }

    /**
     * Metodo que adiciona um novo fornecedor ao programa
     * @param f novo fornecedor a ser adicionado
     * @throws ExisteFornecedorException caso o fornecedor adicionado ja exista
     */
    public void addFornecedor(Fornecedor f) throws ExisteFornecedorException {
        if (this.fornecedores.containsKey(f.getName()))
            throw new ExisteFornecedorException("Este fornecedor já existe");
        this.fornecedores.put(f.getName(), f.clone());
    }

    /**
     * Metodo que muda um fornecedor de uma certa casa
     * @param casa nif da casa onde se pretende alterar o fornecedor
     * @param fornecedor novo fornecedor da casa
     * @throws CasaInexistenteException caso a casa nao exista
     * @throws FornecedorInexistenteException caso nao exista o fornecedor
     */
    public void mudaFornecedorCasa(String casa, String fornecedor) throws CasaInexistenteException, FornecedorInexistenteException{
        Casa c = this.casas.get(casa);
        if(c == null) throw new CasaInexistenteException("Nao existe esta casa");
        if(!this.fornecedores.containsKey(fornecedor)) throw new FornecedorInexistenteException("Nao existe este Fornecedor");
        c.setFornecedor(fornecedor);
    }

    /**
     * Metodo que remove uma casa do programa
     * @param nif nif da casa a remover
     * @throws CasaInexistenteException caso a casa nao exista no programa
     */
    public void removeCasa(String nif) throws CasaInexistenteException {
        if(this.casas.remove(nif) == null) throw new CasaInexistenteException("Nao existe casa com o nif de " + nif);
    }

    /**
     * Metodo que retorna a lista com os nifs do programa
     * @return lista com os nifs do programa
     */
    public List<String> getListNIFs(){
        return new ArrayList<>(this.casas.keySet());
    }

    /**
     * Metodo que retorna lista de casas do programa
     * @return lista de casas
     */
    public List<Casa> listaCasas(){
        return this.casas.values().stream().map(Casa::clone).collect(Collectors.toList());
    }

    /**
     * Metodo que retona a informacao de uma casa
     * @param nif nif do proprietario da casa desejada
     * @return Casa requerida
     * @throws CasaInexistenteException caso nao exista nenhuma casa em que o proprietario tenha o nif passado
     */
    public Casa getCasa(String nif) throws CasaInexistenteException{
        Casa c = this.casas.get(nif);
        if(c == null) throw new CasaInexistenteException("Nao existe casa com NIF " + nif);
        return c.clone();
    }

    /**
     * Metodo que retorna a lista de faturas da casa
     * @param nif nif do proprietario da casa
     * @return Lista com as faturas da casa
     * @throws CasaInexistenteException caso nao exista nenhuma casa com o nif associado
     */
    public List<Fatura> faturasCasa(String nif) throws CasaInexistenteException{
        if(!this.casas.containsKey(nif)) throw new CasaInexistenteException("Nao existe casa com o nif de " + nif);
        return this.casas.get(nif).getFaturas();
    }

    /**
     * Metodo que retorna a lista de fornecedores disponiveis no programa
     * @return lista com os fornecedores do programa
     */
    public List<Fornecedor> getListFornecedores(){
        return this.fornecedores.values().stream().map(Fornecedor::clone).collect(Collectors.toList());
    }

    /**
     * Metodo que calcula a informacao de um dado fornecedor
     * @param nome nome do fornecedor
     * @return copia do fornecedor
     * @throws FornecedorInexistenteException caso nao exista o fornecedor
     */
    public Fornecedor getFornecedor(String nome) throws FornecedorInexistenteException{
        Fornecedor f = this.fornecedores.get(nome);
        if(f == null) throw new FornecedorInexistenteException("Não existe nenhum fornecedor com o nome: " + nome);
        return f;
    }

    /**
     * Metodo para alterar o valor de desconto de um dado fornecedor
     * @param nome nome do fornecedor
     * @param desconto desconto a ser colocado
     * @throws FornecedorInexistenteException caso nao exista o fornecedor
     */
    public void mudaDescontoFornecedor(String nome, double desconto) throws FornecedorInexistenteException{
        this.fornecedores.get(nome).setDesconto(desconto);
    }

    /**
     * Metodo que calcula o ‘top’ 3 de categoria de dispositivos mais usados no programa
     * @return lista com as categorias de dispositivos ordenados por uso
     */
    public List<String> podiumDeviceMaisUsado(){
        return this.casas.values()
                .stream()
                .flatMap(c -> c.getListDevices()
                        .stream()
                        .map(d -> d.getClass().getSimpleName()))
                .collect(Collectors.groupingBy(d  -> d, Collectors.counting()))
                .entrySet().stream()
                .sorted((e1, e2) -> (int) (e2.getValue() - e1.getValue())).map(Map.Entry::getKey).limit(3).collect(Collectors.toList());
    }

    /**
     * Metodo que efetua o comportamento do Consumer passado a um smartBulb da casa
     * @param nif nif da casa onde se encontra o SmartBulb
     * @param id id do dispositivo
     * @param mapperBulb comportamento que se deseja realizar no SmartBulb
     * @throws CasaInexistenteException caso a casa nao exista no programa
     * @throws DeviceInexistenteException caso o dispositivo nao exista no programa
     * @throws TipoDeviceErradoException caso o tipo do dispositivo com o id nao seja SmartBulb
     */
    public void alteraInfoBulbCasa(String nif, int id, Consumer<SmartBulb> mapperBulb) throws CasaInexistenteException, DeviceInexistenteException, TipoDeviceErradoException{
      if(!this.casas.containsKey(nif)) throw new CasaInexistenteException("Não existe casa com o nif de " + nif);

      this.casas.get(nif).alteraInfoBulb(id, mapperBulb);

    }


    /**
     * Metodo que efetua o comportamento do Consumer passado a um SmartSpeaker da casa
     * @param nif nif da casa onde se encontra o SmartSpeaker
     * @param id id do dispositivo
     * @param mapperSpeaker comportamento que se deseja realizar no SmartSpeaker
     * @throws CasaInexistenteException caso a casa nao exista no programa
     * @throws DeviceInexistenteException caso o dispositivo nao exista no programa
     * @throws TipoDeviceErradoException caso o tipo do dispositivo com o id nao seja SmartSpeaker
     */
    public void alteraInfoSpeakerCasa(String nif, int id, Consumer<SmartSpeaker> mapperSpeaker) throws DeviceInexistenteException, TipoDeviceErradoException, CasaInexistenteException{

      if(!this.casas.containsKey(nif)) throw new CasaInexistenteException("Não existe casa com o nif de " + nif);

      this.casas.get(nif).alteraInfoSpeaker(id, mapperSpeaker);
    }


    /**
     * Metodo que efetua o comportamento do Consumer passado a um SmartCamera da casa
     * @param nif nif da casa onde se encontra o SmartCamera
     * @param id id do dispositivo
     * @param mapperCamera comportamento que se deseja realizar no SmartCamera
     * @throws CasaInexistenteException caso a casa nao exista no programa
     * @throws DeviceInexistenteException caso o dispositivo nao exista no programa
     * @throws TipoDeviceErradoException caso o tipo do dispositivo com o id nao seja SmartCamera
     */
    public void alteraInfoCameraCasa(String nif, int id, Consumer<SmartCamera> mapperCamera) throws CasaInexistenteException, DeviceInexistenteException, TipoDeviceErradoException{
      if(!this.casas.containsKey(nif)) throw new CasaInexistenteException("Não existe casa com o nif de " + nif);

      this.casas.get(nif).alteraInfoCamera(id, mapperCamera);
    }

    /**
     * Metodo que efetua um dado comportamento na casa desejada
     * @param nif nif da casa
     * @param mapperCasa comportamento a ser realizado na casa
     * @throws CasaInexistenteException caso a casa nao exista
     */
    public void alteraInfoCasa(String nif, Consumer<Casa> mapperCasa) throws CasaInexistenteException{
        if(!this.casas.containsKey(nif)) throw new CasaInexistenteException("Não existe nenhuma casa com este nif: " + nif);

        mapperCasa.accept(this.casas.get(nif));
    }

    public void setData(LocalDate currentDate) {
        this.data_atual=currentDate;
    }
}
