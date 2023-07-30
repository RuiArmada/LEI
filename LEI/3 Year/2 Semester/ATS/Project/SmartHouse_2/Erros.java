public class Erros {

    private Boolean bool;
    private String erro;

    /**
     * Construtor de inicialização das variáveis de instância
     */
    public Erros() {
        bool = false;
        erro = "";
    }

    /**
     * Construtor de inicialização das variáveis de instância
     * 
     * @param bool var. bool
     */
    public Erros(boolean bool) {
        this.bool = bool;
        erro = "";
    }

    /**
     * Construtor de inicialização das variáveis de instância
     * 
     * @param bool var. bool
     * @param erro var. que guarda a mensagem de erro
     */
    public Erros(Boolean bool, String erro) {
        this.bool = bool;
        this.erro = erro;
    }

    /**
     * Atualiza a var. bool
     * 
     * @param bool true se existir/estiver correto, false se não existir/estiver incorreto
     */
    public void setBool(Boolean bool) {
        this.bool = bool;
    }

    /**
     * Atualiza a var. erro
     * 
     * @param erro guarda a mensagem de erro
     */
    public void setErro(String erro) {
        this.erro = erro;
    }

    /**
     * Devolve o valor da var. bool
     * 
     * @return o valor da var. bool
     */
    public Boolean getBool() {
        return bool;
    }

    /**
     * Devolve o valor da var. erro
     * 
     * @return o valor da var. erro
     */
    public String getErro() {
        return erro;
    }

    /**
     * Testa se um fornecedor existe e devolve um erro
     * 
     * @param suppliers var. que tem os fornecedores de energia guardados
     * @param fornecedor var. com o energySupplier que queremos testar se existe ou não
     */
    public void erroExisteFornecedor(Suppliers suppliers, String fornecedor)
    {
        if((suppliers.getSuppliers().containsKey(fornecedor)))
        {
            this.erro = "[ERRO]\n<< Esse fornecedor já existe! >>";
            this.bool = true;
        } else {
            this.erro = "[ERRO]\n<< Esse fornecedor não existe! >>";
            this.bool = false;
        }
    }

    /**
     * Testa se um uma casa inteligente com um certo nif existe e devolve um erro
     * 
     * @param suppliers var. que tem os fornecedores de energia guardados
     * @param fornecedor var. que guarda o nome do fornecedor da casa inteligente que queremos verificar
     * @param nif var. que guarda o nif do dono da casa inteligente
     */
    public void erroExisteCasaInteligente(Suppliers suppliers, String fornecedor, int nif)
    {
        if(suppliers.getSuppliers().get(fornecedor).getHouses().containsKey(nif))
        {
            this.erro = "[ERRO]\n<< Já existe uma casa inteligente com esse nif! >>";
            this.bool = true;
        } else {
            this.erro = "[ERRO]\n<< Não existe uma casa inteligente com esse nif! >>";
            this.bool = false;
        }
    }

    /** 
     * Testa se uma divisão existe e devolve um erro
     * 
     * @param suppliers var. que tem os fornecedores de energia guardados
     * @param fornecedor var. que guarda o nome do fornecedor da casa inteligente que queremos verificar
     * @param nif var. que guarda o nif do dono da casa inteligente
     * @param divisao var. que guarda o nomo da divisão que queremos verificar
     */
    public void erroExisteDivisao(Suppliers suppliers, String fornecedor, int nif, String divisao) 
    {
        if(suppliers.getSupplier(fornecedor).getHouse(nif).getLocations().containsKey(divisao))
        {
            this.erro = "[ERRO]\n<< Já existe uma divisão com esse nome! >>";
            this.bool = true;
        } else {
            this.erro = "[ERRO]\n<< Não existe uma divisão com esse nome! >>";
            this.bool = false;
        }
    }

    /**
     * Testa se um device com um certo id existe e devolve um erro
     * 
     * @param suppliers var. que tem os fornecedores de energia guardados
     * @param fornecedor var. que guarda o nome do fornecedor da casa inteligente que queremos verificar
     * @param nif var. que guarda o nif do dono da casa inteligente
     * @param id var. que guarda o if do device que queremos verificar se existe
     */
    public void erroExisteDeviceID(Suppliers suppliers, String fornecedor, int nif, int id)
    {
        if(suppliers.getSupplier(fornecedor).getHouse(nif).getDevices().containsKey(id))
        {
            this.erro = "[ERRO]\n<< Já existe um dispositivo com esse ID! >>";
            this.bool = true;
        } else {
            this.erro = "[ERRO]\n<< Não existe um dispositivo com esse ID! >>";
            this.bool = false;
        }
    }

    /**
     * 
     * Testa se o preço antigo do fornecedor é igual ao novo
     * 
     * @param suppliers var. que tem os fornecedores de energia guardados
     * @param fornecedor var. que guarda o nome do fornecedor que queremos verificar o seu preço
     * @param novo var. que guarda o novo preço do fornecedor
     */
    public void erroPriceFornecedor(Suppliers suppliers, String fornecedor, double novo) 
    {
        if(suppliers.getSupplier(fornecedor).getPrice()==novo)
        {
            this.erro = "[ERRO]\n<< O novo preço é igual ao antigo! >>";
            this.bool = true;
        } else {
            this.erro = "[ERRO]\n<< O novo preço não é igual ao antigo! >>";
            this.bool = false;
        }
    }

    /**
     * Testa se o imposto antigo do fornecedor é igual ao novo
     * 
     * @param suppliers var. que tem os fornecedores de energia guardados
     * @param fornecedor var. que guarda o nome do fornecedor que queremos verificar o seu imposto
     * @param novo var. que guarda o novo imposto do fornecedor
     */
    public void erroTaxesFornecedor(Suppliers suppliers, String fornecedor, double novo) 
    {
        if(suppliers.getSupplier(fornecedor).getTaxes()==novo)
        {
            this.erro = "[ERRO]\n<< O novo imposto é igual ao antigo! >>";
            this.bool = true;
        } else {
            this.erro = "[ERRO]\n<< O novo imposto não é igual ao antigo! >>";
            this.bool = false;
        }
    }

    /**
     * Devolve erro se houver um problema no menu faturação
     * 
     * @param suppliers var. que tem os fornecedores de energia guardados
     * @param fornecedor var. que guarda o nome do fornecedor que queremos verificar
     */
    public void erroFaturacao(Suppliers suppliers, String fornecedor)
    {
        if(suppliers.getSuppliers().containsKey(fornecedor)==false)
        {
            this.erro = "[ERRO]\n<< Erro ao calcular o volume de faturação! >>";
            this.bool = true;
        }
    }

    /**
     * Método que devolve a representação em String do erro
     * 
     * @return String com o erro
     */
    public String toString() {
        return this.erro; 
    }
}
