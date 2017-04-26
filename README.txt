This is the README from FortranAnalyser

Author: Michael García Rodríguez
Environmental Physics Laboratory - University of Vigo, Ourense.


================================================================================ 
=                               DESCRIPTION                                    =
================================================================================
   This application analyse all files from a specific directory and search all
 FORTRAN files. This files extension are ".f90" and ".h90". For each files the
 program:

    - count the number of lines from the file.
        It is a question of verifying the magnitude of the file, since the 
        greater the number of lines has a program, the more complex it is.

    - count the number of variables declared.
        Each declared variable will be a buffer of the system memory so that
        the fewer variables are declared in the class, the better.

    - count the number of subrutines calls
        The greater the number of calls to subroutines, the greater the
        complexity of the code. Therefore, the execution time and the number of
        resources to be used to execute it will be greater.

    - check the use of the sentence "implicit none". 
        There is a convention for the implicit declaration of integer and real 
        variables. It is advisable, however, to declare all the variables of a 
        program, making use of the declarations corresponding to the type that 
        each object with which it works. Therefore, to avoid ambiguity and 
        risks, Fortran 90 has a statement to override any specification of the 
        implicit type. When using this statement, any occurrence of a variable 
        whose type has not been explicitly declared will be flagged as an error 
        by the compiler.

    - check if the document have comments
        A good practice of programation is comment all the program. The use of 
        comments constitutes good programming practice. Comments are very 
        important when developing the code since it makes it easier for 
        developers to understand the code. This improves the legibility of the 
        code, in addition to its structuring, allowing a quicker and easier 
        understanding of what the code that is being read is doing.


================================================================================ 
=                              INSTRUCTIONS                                    =
================================================================================

Execute the program and you can visualice the follow buttons: 

    - "Analyse": this button first check if a directory is selected. After that, 
                 it execute the analysis of the selected directory. It produce a
                 report file saved in the directory "./tmp/QualityInform.pdf".

    - "Exit": This button close the application.

    - "...": This button open a file explorer to select the directory to be 
             analyzed.

    -