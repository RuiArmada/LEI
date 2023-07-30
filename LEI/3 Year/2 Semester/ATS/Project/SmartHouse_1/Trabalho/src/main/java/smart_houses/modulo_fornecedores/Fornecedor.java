package smart_houses.modulo_fornecedores;

import smart_houses.EstadoPrograma;
import smart_houses.Fatura;
import smart_houses.exceptions.FornecedorErradoException;
import smart_houses.modulo_casas.Casa;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static java.time.temporal.ChronoUnit.DAYS;

/**
 * Classe Fornecedor que representa o fornecedor das casas
 */
public class Fornecedor implements Serializable {

    // Nome do fornecedor
    private String name;

    // percentagem que o fornecedor tem como desconto
    private double desconto;

    // lista com as faturas que o fornecedor criou
    private List<Fatura> faturas;


    //Construtor de omissão de Fornecedor
    public Fornecedor() {
        this.desconto = 0.1;
        this.name = "n/a";
        this.faturas = new ArrayList<>();
    }

    /**
     * Contrutor parametrizado do fornecedor
     * @param name nome do fornecedor
     */
    public Fornecedor(String name){
        this.name = name;
        this.desconto = 0.1;
        this.faturas = new ArrayList<>();
    }

    /**
     * contrutor parametrizado do fornecedor
     * @param name nome do fornecedor
     * @param desconto valor do desconto do fornecedor
     */
    public Fornecedor(String name, double desconto){
        this.name = name;
        this.desconto = desconto;
        this.faturas = new ArrayList<>();
    }

    /**
     * Contrutor de cópia
     * @param fornecedor fornecedor que pretende copiar
     */
    public Fornecedor(Fornecedor fornecedor){
        this.desconto = fornecedor.getDesconto();
        this.name=fornecedor.getName();
        this.faturas = fornecedor.getFaturas();
    }

    /**
     * @return lista com cópia das faturas do fornecedor
     */
    public List<Fatura> getFaturas() {
        return this.faturas.stream().map(Fatura::clone).collect(Collectors.toList());
    }

/**
 * Método que copia todas as faturas existentes no fornecedor, guardando-as numa lista e retornando-a
 * @param faturas lista com a cópia das faturas do fornecedor
 */
    public void setFaturas(List<Fatura> faturas) {
        this.faturas = faturas.stream().map(Fatura::clone).collect(Collectors.toList());
    }

    /**
     * Metodo que cria a fatura para uma casa dado periodo e manda uma excecao caso o fornecedor nao corresponda ao fornecedor da casa
     * @param casa casa da qual se quer emitir a fatura
     * @param inicio periodo inicial da fatura
     * @param fim periodo final da fatura
     * @return fatura gerada
     * @throws FornecedorErradoException caso o fornecedor da casa nao corresponda ao fornecedor desta instancia
     */
    public Fatura criaFatura(Casa casa, LocalDate inicio, LocalDate fim) throws FornecedorErradoException {
        if(!casa.getFornecedor().equals(this.name)) throw new FornecedorErradoException("Este nao é o fornecedor desta casa, casa = " + casa);
        long days = DAYS.between(inicio, fim);
        double consumo = casa.consumoDispositivos();
        double preco = this.precoDia(consumo, casa.getMapDevices().size()) * days;
        consumo *= days;
        return new Fatura(this.name, casa.getNif(), preco, consumo, inicio, fim);
    }

    /**
     * Metodo que retorna o valor da variavel de instancia desconto
     * @return valor da variavel de instancia desconto
     */
    public double getDesconto() {
        return desconto;
    }

    /**
     * Método que coloca um valor novo de desconto na variavel de instancia desconto
     * @param desconto valor novo de desconto a colocar na variavel de instancia desconto
     */
    public void setDesconto(double desconto) {
        this.desconto = desconto;
    }

    /**
     * Metodo que retorna o valor que se encontra na variavel de instancia nome
     * @return valor da variavel de instancia nome
     */
    public String getName() {
        return name;
    }

    /**
     * Metodo que coloca o valor da variavel de instancia nome, dado um nome
     * @param name valor a colocar na variavel de instancia nome
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Metodo que calcula uma string com a representacao do objeto
     * @return string que representa o objeto
     */
    public String toString() {
        return "Fornecedor{" +
                "name='" + name + '\'' +
                ", desconto=" + desconto + '\'' +
                ", faturas=" + this.faturas +
                '}';
    }

    /**
     * Metodo que compara o objeto com um outro objeto
     * @param o objeto com o qual se vai comparar
     * @return true se o objeto for igual, false caso contrario
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Fornecedor that = (Fornecedor) o;

        return this.getName().equals(that.getName()) && this.desconto == that.getDesconto() && this.faturas.equals(that.getFaturas());
    }

    /**
     * Metodo que calcula o valor de hash do objeto
     * @return valor de hash do objeto
     */
    public int hashCode() {
        int r = 7;
        r = r * 31 + this.name.hashCode();
        r = r * 31 + Double.hashCode(this.desconto);
        r = r * 31 + this.faturas.hashCode();
        return r;
    }

    /**
     * Metodo que retorna o custo para um dado consumo e numero de dispositivos
     * @param consumo valor de um consumo kWh
     * @param n_devices numero de devices
     * @return valor do custo
     */
    private double precoDia(double consumo, int n_devices){
        double precoSDesc = (n_devices < 7) ? EstadoPrograma.custoEnergia * consumo * (1 + EstadoPrograma.imposto) * 0.9 : EstadoPrograma.custoEnergia * consumo * (1 + EstadoPrograma.imposto) * 0.6;
        double desc = precoSDesc * this.desconto;
        return precoSDesc - desc;
    }

    /**
     * Metodo que retorna uma copia do objeto
     * @return copia do objeto
     */
    public Fornecedor clone(){
        return new Fornecedor(this);
    }

    /**
     * Metodo que adiciona a copia de uma fatura à collection de faturas
     * @param f fatura a adicionar
     */
    public void adicionaFatura(Fatura f){
        this.faturas.add(f.clone());
    }

    /**
     * Metodo que calcula a faturacao total do fornecedor atraves de todas as suas faturas
     * @return faturacao total do fornecedor
     */
    public double faturacao(){
        return this.faturas.stream().mapToDouble(Fatura::getCusto).sum();
    }

}
