package smart_houses.exceptions;

public class ExisteCasaException extends Exception{
    public ExisteCasaException(String msg){
        super(msg);
    }

    public String getMensagem(){
        return this.getMessage();
    }
}
