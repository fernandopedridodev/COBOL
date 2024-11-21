       IDENTIFICATION DIVISION.
       PROGRAM-ID. SistemaNomina.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       * Declaración de constantes
       01 MAX-EMPLEADOS      PIC 9(2) VALUE 20.
       01 TASA-IMPUESTOS     PIC 9(3)V99 VALUE 0.15.  * 15% de impuestos
       01 TASA-DEDUCCIONES   PIC 9(3)V99 VALUE 0.05.  * 5% de deducciones

       * Contador de empleados
       01 CONTADOR           PIC 9(2) VALUE 0.

       * Estructura para almacenar empleados
       01 EMPLEADOS.
           05 LISTA OCCURS 20 TIMES INDEXED BY INDICE.
               10 NOMBRE         PIC X(20).
               10 HORAS-TRAB    PIC 9(3).
               10 TARIFA-HORA   PIC 9(3)V99.
               10 SALARIO-BRUTO PIC 9(5)V99.
               10 SALARIO-NETO  PIC 9(5)V99.

       * Variables temporales
       01 TEMP-NOMBRE         PIC X(20).
       01 TEMP-HORAS          PIC 9(3).
       01 TEMP-TARIFA         PIC 9(3)V99.
       01 TEMP-BRUTO          PIC 9(5)V99.
       01 TEMP-NETO           PIC 9(5)V99.
       01 OPCION              PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "=== SISTEMA DE NÓMINA ===".
           PERFORM MENU.

           DISPLAY "Gracias por usar el sistema.".
           STOP RUN.

       MENU.
           PERFORM UNTIL OPCION = 3
               DISPLAY "1. Agregar Empleado".
               DISPLAY "2. Mostrar Nómina".
               DISPLAY "3. Salir".
               DISPLAY "Seleccione una opción: ".
               ACCEPT OPCION.

               EVALUATE OPCION
                   WHEN 1
                       PERFORM AGREGAR-EMPLEADO
                   WHEN 2
                       PERFORM MOSTRAR-NOMINA
                   WHEN 3
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Opción inválida, intente de nuevo."
               END-EVALUATE
           END-PERFORM.

       AGREGAR-EMPLEADO.
           IF CONTADOR >= MAX-EMPLEADOS THEN
               DISPLAY "No se pueden agregar más empleados. El sistema está lleno."
           ELSE
               ADD 1 TO CONTADOR
               SET INDICE TO CONTADOR

               DISPLAY "Ingrese el nombre del empleado: ".
               ACCEPT TEMP-NOMBRE.
               MOVE TEMP-NOMBRE TO NOMBRE(INDICE).

               DISPLAY "Ingrese las horas trabajadas: ".
               ACCEPT TEMP-HORAS.
               MOVE TEMP-HORAS TO HORAS-TRAB(INDICE).

               DISPLAY "Ingrese la tarifa por hora: ".
               ACCEPT TEMP-TARIFA.
               MOVE TEMP-TARIFA TO TARIFA-HORA(INDICE).

               * Calcular salario bruto
               COMPUTE TEMP-BRUTO = TEMP-HORAS * TEMP-TARIFA.
               MOVE TEMP-BRUTO TO SALARIO-BRUTO(INDICE).

               * Calcular salario neto
               COMPUTE TEMP-NETO = TEMP-BRUTO - (TEMP-BRUTO * TASA-IMPUESTOS)
                                   - (TEMP-BRUTO * TASA-DEDUCCIONES).
               MOVE TEMP-NETO TO SALARIO-NETO(INDICE).

               DISPLAY "Empleado agregado exitosamente.".
           END-IF.

       MOSTRAR-NOMINA.
           IF CONTADOR = 0 THEN
               DISPLAY "No hay empleados registrados en la nómina."
           ELSE
               DISPLAY "=== REPORTE DE NÓMINA ===".
               DISPLAY "Nombre             Horas  Tarifa   Bruto   Neto".
               DISPLAY "----------------------------------------------".

               PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > CONTADOR
                   DISPLAY NOMBRE(INDICE) SPACE
                           HORAS-TRAB(INDICE) SPACE
                           TARIFA-HORA(INDICE) SPACE
                           SALARIO-BRUTO(INDICE) SPACE
                           SALARIO-NETO(INDICE)
               END-PERFORM.

               DISPLAY "----------------------------------------------".
           END-IF.
