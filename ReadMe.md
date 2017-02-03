# Delphi: Creación de ficheros normas 19.14 y 34.14 SEPA formato XML

Encontrará dos clases para la creación de los siguientes ficheros:

- Norma 19.14 SEPA XML. Ordenes de cobro. El **ordenante** cobra al **deudor**. Internamente tenemos una lista de ordenantes, y asociada a cada uno de los miembros una lísta de órdenes de Cobro.
  Debemos instanciar tanto la clase Ordenante (TsepaInitiator) como cada uno de los cobors (TsepaOperation) del modo tradicional. Se incluye un ejemplo que facilita la comprensión.

- Norma 34.14 SEPA XML. Es un fichero de órdenes de pago. El **ordenante** paga al **beneficiario**.

Algunos puntos importantes a tener en cuenta.

- **Lean la normativa acerca de la generación de estos ficheros**. Es compleja. Hay campos opcionales que no se han añadido aquí, pero que es posible que su banco exija.
 Los identificadores únicos de cada elemento, son importantes, hay que leer su significado y tomar la decisión de cómo formarlos.
 Por si fuera poco, cada banco hace su propia interpretación de la normativa y puede que le necesite hacer algunas variaciones según el caso
 **No se trata de un componente "listo para el uso"**. Son clases que hay que instanciar y manejar. Lo que si facilita es la estructuración y la escritura de las etiquetas.

- Como guardamos la info en Listas Genéricas, no existes límmites de ningún tipo. Supongo que los bancos tampoco los pondrán al leer los datos.

- En la normativa hay muchos campos opcionales, no se ha añadido ninguno.

- Se trata de los esquemas básicos, no los b2b.

- Solamente se contemplan transferencias en euros, nada de cheques.

- No se contemplan devoluciones, etc.

- **No se hace ningún chequeo de contenidos (IBAN, BIC, etc)** (al menos en esta versión).

- Vea el proyecto test de ejemplo.

La normativa: 
- Para la norma 34.14, buscar **Órdenes en formato ISO 20022 para emisión de transferencias y cheques en euros**
por ejemplo [https://empresa.lacaixa.es/deployedfiles/empresas/Estaticos/pdf/Transferenciasyficheros/Cuaderno_34_XML_Noviembre_2015.pdf](https://empresa.lacaixa.es/deployedfiles/empresas/Estaticos/pdf/Transferenciasyficheros/Cuaderno_34_XML_Noviembre_2015.pdf)
Puede ir directament al **ANEXO 1**. La programación de este proyecto se realizó siguiendo este ANEXO.
¡Ojo! hay algunos documentos desactualizados. ¡Ponga atención a las fechas! El último que he encontrado es de Noviembre 2015

- Para la norma 19.14 buscar **Órdenes en formato ISO 20022 para emisión de adeudos directos SEPA en euros**
por ejemplo [https://empresa.lacaixa.es/deployedfiles/empresas/Estaticos/pdf/Transferenciasyficheros/CuadernoXMLSDDCoreFebrero2014.pdf](https://empresa.lacaixa.es/deployedfiles/empresas/Estaticos/pdf/Transferenciasyficheros/CuadernoXMLSDDCoreFebrero2014.pdf)

Actualización (Febrero 2016): Testeado en 3 bancos españoles. Tanto la norma 19.14 como la 34.14 y los ficheros han sido aceptados.
Actualización (Febrero 2016): Testeado en 3 bancos españoles. Tanto la norma 19.14 como la 34.14 y los ficheros han sido aceptados.

Espero que os sea de utilidad.
Juan C.Cilleruelo Gonzalo


senCille.es

**MANUFACTURING SOFTWARE**.