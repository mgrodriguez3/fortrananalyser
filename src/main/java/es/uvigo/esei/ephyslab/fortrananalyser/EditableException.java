/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser;

/**
 * Exception class with all EditableException
 *
 * @author Michael García Rodríguez
 * @version 1.9.2
 */
public class EditableException extends Exception{
    
    /**
     * code number of the exception error
     */
    private final int errorCode;
     
    public EditableException(int codigoError){
        super();
        this.errorCode=codigoError;
    }
     
    
    @Override
    public String getMessage(){
         
        String errorMessage="";
         
        switch(errorCode){
            case 111:
                errorMessage="bad number of argument. Check README.md file \n";
                break;
                
            default: 
                errorMessage="Generic Exception Error \n";
                break;
        }
         
        return errorMessage;
         
    }
    
}
