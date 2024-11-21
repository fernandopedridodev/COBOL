 IDENTIFICATION DIVISION.
       PROGRAM-ID. ParImpar.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMERO      PIC 9(4).
       01 RESTO       PIC 9.
       PROCEDURE DIVISION.
           DISPLAY "Ingrese un número: ".
           ACCEPT NUMERO.
           COMPUTE RESTO = NUMERO MOD 2.
           IF RESTO = 0 THEN
               DISPLAY "El número es par."
           ELSE
               DISPLAY "El número es impar."
           END-IF.
           STOP RUN.