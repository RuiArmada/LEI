package smart_houses.exceptions;

public class ExisteFornecedorException extends Exception{
    public ExisteFornecedorException(String msg){
        super(msg);
    }

    public String getMensagem(){
        return this.getMessage();
    }
}
