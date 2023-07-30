package smart_houses;

import java.io.Serializable;
import java.time.LocalDate;

/**
 * Classe que representa as faturas
 */
public class Fatura implements Comparable<Fatura>, Serializable {

    // Variavel estatica para gerar um codigo unico para cada fatura
    public static int next_codigoFatura = 1;

    // codigo da fatura
    private final int codigoFatura;

    // nome do fornecedor que gerou a fatura
    private final String fornecedor;

    // nif do cliente da fatura
    private final String nifCliente;
  
    // Custo associado à fatura
    private final double custo;

    // Consumo associado à fatura
    private final double consumo;
    
    // Inicio do periodo da fatura
    private final LocalDate inicioPeriodo;

    // Fim do periodo da fatura
    private final LocalDate fimPeriodo;

    //Contrutor por omissao da fatura
    public Fatura(){
        this.codigoFatura = Fatura.next_codigoFatura++;
        this.fornecedor = "";
        this.nifCliente = "";
        this.custo = 0;
        this.consumo = 0;
        this.inicioPeriodo = LocalDate.now();
        this.fimPeriodo = LocalDate.now();
    }

    // Construtor parametrizado da fatura
    public Fatura(String fornecedor, String nifCliente, double custo, double consumo, LocalDate inicioPeriodo, LocalDate fimPeriodo) {
        this.codigoFatura = Fatura.next_codigoFatura++;
        this.fornecedor = fornecedor;
        this.nifCliente = nifCliente;
        this.custo = custo;
        this.consumo = consumo;
        this.inicioPeriodo = inicioPeriodo;
        this.fimPeriodo = fimPeriodo;
    }

/**
 * Contrutor de copia da fatura
 * @param f fatura que pretende copiar
 */
    public Fatura(Fatura f){
        this.codigoFatura = f.getCodigoFatura();
        this.fornecedor = f.getFornecedor();
        this.nifCliente = f.getNifCliente();
        this.custo = f.getCusto();
        this.consumo = f.getConsumo();
        this.inicioPeriodo = f.getInicioPeriodo();
        this.fimPeriodo = f.getFimPeriodo();
    }

/**
 * > Metodo que retorna o valor da variavel de instancia codigo
 * 
 * @return O valor da variavel de instancia codigo
 */
    public int getCodigoFatura() {
        return codigoFatura;
    }


    /**
     * Metodo que retorna o valor da variavel de instancia fornecedor do objeto
     * 
     * @return Valor da variavel fornecedor
     */
    public String getFornecedor() {
        return fornecedor;
    }

    /**
     * Metodo que retorna o valor da variavel do nif do cliente 
     * 
     * @return Valor do nif do cliente
     */
    public String getNifCliente() {
        return nifCliente;
    }

    /**
     * Metodo que retorna o custo associado na fatura
     * 
     * @return O custo de energia referente na fatura
     */
    public double getCusto() {
        return custo;
    }

    /**
     * Metodo que retorna o consumo associado na fatura
     *
     * @return O consumo de energia referente na fatura
     */
    public double getConsumo() {
        return consumo;
    }

    /**
     * Metodo que retorna a data do inicio do periodo da fatura
     *
     * @return valor do inicio do periodo da fatura
     */
    public LocalDate getInicioPeriodo() {
        return inicioPeriodo;
    }

    /**
     * Metodo que retorna a data do fim do periodo da fatura
     *
     * @return valor do fim do periodo da fatura
     */
    public LocalDate getFimPeriodo() {
        return fimPeriodo;
    }

    /**
     * Metodo que calcula a representacao do objeto numa string
     * 
     * @return String com a representacao do objeto
     */
    public String toString() {
        return "Fatura{" +
                "codigoFatura=" + codigoFatura +
                ", fornecedor='" + fornecedor + '\'' +
                ", nifCliente='" + nifCliente + '\'' +
                ", custo=" + custo +
                ", consumo=" + consumo +
                ", inicioPeriodo=" + inicioPeriodo +
                ", fimPeriodo=" + fimPeriodo +
                '}';
    }

    /**
     * Metodo que compara os dois objetos
     * @param o Objeto a ser comparado
     * @return retorna o valor de se sao iguais ou nao
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Fatura fatura = (Fatura) o;

        if (getCodigoFatura() != fatura.getCodigoFatura()) return false;
        if (Double.compare(fatura.getCusto(), getCusto()) != 0) return false;
        if (Double.compare(fatura.getConsumo(), getConsumo()) != 0) return false;
        if (!getFornecedor().equals(fatura.getFornecedor())) return false;
        if (!getNifCliente().equals(fatura.getNifCliente())) return false;
        if (!getInicioPeriodo().equals(fatura.getInicioPeriodo())) return false;
        return getFimPeriodo().equals(fatura.getFimPeriodo());
    }

    /**
     * Metodo que calcula o codigo hash do objeto
     * 
     * @return Codigo hash do objeto
     */
    public int hashCode() {
        int result;
        long temp;
        result = getCodigoFatura();
        result = 31 * result + getFornecedor().hashCode();
        result = 31 * result + getNifCliente().hashCode();
        temp = Double.doubleToLongBits(getCusto());
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(getConsumo());
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        result = 31 * result + getInicioPeriodo().hashCode();
        result = 31 * result + getFimPeriodo().hashCode();
        return result;
    }

    /**
     * Metodo de ordenacao natural da fatura, sendo este pelo periodo inicial da mesma
     *
     * @param f objeto a ser comparado
     * @return diferenca entre as duas datas
     */
    public int compareTo(Fatura f){
        return this.inicioPeriodo.compareTo(f.getInicioPeriodo());
    }

    /**
     * Metodo que copia o objeto
     * 
     * @return Copia do objeto
     */
    public Fatura clone(){
        return new Fatura(this);
    }
}
