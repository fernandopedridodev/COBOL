  IDENTIFICATION DIVISION.
       PROGRAM-ID. SumaNumeros.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1      PIC 9(3).
       01 NUM2      PIC 9(3).
       01 RESULTADO PIC 9(4).
       PROCEDURE DIVISION.
           DISPLAY "Ingrese el primer número: ".
           ACCEPT NUM1.
           DISPLAY "Ingrese el segundo número: ".
           ACCEPT NUM2.
           COMPUTE RESULTADO = NUM1 + NUM2.
           DISPLAY "El resultado de la suma es: " RESULTADO.
           STOP RUN.
