       IDENTIFICATION DIVISION.
       PROGRAM-ID. SistemaInventario.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       * Declaración de constantes
       01 MAX-PRODUCTOS       PIC 9(2) VALUE 10.
       
       * Contador de productos actuales
       01 CONTADOR            PIC 9(2) VALUE 0.
       
       * Estructura para almacenar productos
       01 PRODUCTOS.
           05 LISTA OCCURS 10 TIMES INDEXED BY INDICE.
               10 CODIGO     PIC X(10).
               10 NOMBRE     PIC X(20).
               10 PRECIO     PIC 9(5)V99.
               10 CANTIDAD   PIC 9(3).

       * Variables para entrada de datos
       01 OPCION             PIC 9 VALUE 0.
       01 TEMP-CODIGO        PIC X(10).
       01 TEMP-NOMBRE        PIC X(20).
       01 TEMP-PRECIO        PIC 9(5)V99.
       01 TEMP-CANTIDAD      PIC 9(3).
       01 ENCONTRADO         PIC X VALUE "N".

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "=== SISTEMA DE INVENTARIO ===".
           PERFORM MENU.

           DISPLAY "Gracias por usar el sistema.".
           STOP RUN.

       MENU.
           PERFORM UNTIL OPCION = 4
               DISPLAY "1. Agregar Producto".
               DISPLAY "2. Mostrar Inventario".
               DISPLAY "3. Buscar Producto".
               DISPLAY "4. Salir".
               DISPLAY "Seleccione una opción: ".
               ACCEPT OPCION.

               EVALUATE OPCION
                   WHEN 1
                       PERFORM AGREGAR-PRODUCTO
                   WHEN 2
                       PERFORM MOSTRAR-INVENTARIO
                   WHEN 3
                       PERFORM BUSCAR-PRODUCTO
                   WHEN 4
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Opción inválida, intente de nuevo."
               END-EVALUATE
           END-PERFORM.

       AGREGAR-PRODUCTO.
           IF CONTADOR >= MAX-PRODUCTOS THEN
               DISPLAY "El inventario está lleno. No se pueden agregar más productos."
           ELSE
               ADD 1 TO CONTADOR
               SET INDICE TO CONTADOR

               DISPLAY "Ingrese el código del producto: ".
               ACCEPT TEMP-CODIGO.
               MOVE TEMP-CODIGO TO CODIGO(INDICE).

               DISPLAY "Ingrese el nombre del producto: ".
               ACCEPT TEMP-NOMBRE.
               MOVE TEMP-NOMBRE TO NOMBRE(INDICE).

               DISPLAY "Ingrese el precio del producto: ".
               ACCEPT TEMP-PRECIO.
               MOVE TEMP-PRECIO TO PRECIO(INDICE).

               DISPLAY "Ingrese la cantidad en stock: ".
               ACCEPT TEMP-CANTIDAD.
               MOVE TEMP-CANTIDAD TO CANTIDAD(INDICE).

               DISPLAY "Producto agregado exitosamente.".
           END-IF.

       MOSTRAR-INVENTARIO.
           IF CONTADOR = 0 THEN
               DISPLAY "El inventario está vacío."
           ELSE
               DISPLAY "=== INVENTARIO ===".
               PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > CONTADOR
                   DISPLAY "Código: " CODIGO(INDICE)
                   DISPLAY "Nombre: " NOMBRE(INDICE)
                   DISPLAY "Precio: " PRECIO(INDICE)
                   DISPLAY "Cantidad: " CANTIDAD(INDICE)
                   DISPLAY "----------------------"
               END-PERFORM.
           END-IF.

       BUSCAR-PRODUCTO.
           DISPLAY "Ingrese el código del producto a buscar: ".
           ACCEPT TEMP-CODIGO.
           MOVE "N" TO ENCONTRADO.

           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > CONTADOR OR ENCONTRADO = "S"
               IF CODIGO(INDICE) = TEMP-CODIGO THEN
                   DISPLAY "Producto encontrado: "
                   DISPLAY "Código: " CODIGO(INDICE)
                   DISPLAY "Nombre: " NOMBRE(INDICE)
                   DISPLAY "Precio: " PRECIO(INDICE)
                   DISPLAY "Cantidad: " CANTIDAD(INDICE)
                   MOVE "S" TO ENCONTRADO
               END-IF
           END-PERFORM.

           IF ENCONTRADO = "N" THEN
               DISPLAY "Producto no encontrado."
           END-IF.
