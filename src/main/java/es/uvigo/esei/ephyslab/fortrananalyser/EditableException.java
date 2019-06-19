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
    
    private final int errorCode;
     
    public EditableException(int codigoError){
        super();
        this.errorCode=codigoError;
    }
     
    
    @Override
    public String getMessage(){
         
        String mensaje="";
         
        switch(errorCode){
            case 111:
                mensaje="bad number of argument. Check README.md file \n";
                break;
            case 222:
                mensaje="Error, el numero esta entre 11 y 20";
                break;
            case 333:
                mensaje="Error, el numero esta entre 21 y 30";
                break;
        }
         
        return mensaje;
         
    }
    
    
}
