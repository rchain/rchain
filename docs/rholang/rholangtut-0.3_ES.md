# Un tutorial de Rholang

Rholang es un nuevo lenguaje de programación diseñado para ser usado en sistemas distribuidos. Como todas las cosas nuevas, está creciendo y cambiando rápidamente. Este documento describe la sintaxis que se usará en la versión RNode-0.3.

Rholang es un lenguaje "orientado al proceso": toda la computación se realiza mediante el envío de mensajes. Los mensajes son transmitidos por "canales", que son más bien como colas de mensajes, pero se comportan como conjuntos en vez de colas. Rholang es completamente asíncrono, en el sentido de que aunque puede leer un mensaje de un canal y después hacer algo con él, no puede enviar un mensaje y luego hacer algo una vez que ha sido recibido, al menos, no sin esperar explícitamente un mensaje de acuse de recibo del receptor. Tenga en cuenta que a lo largo de este documento, las palabras "nombre" y "canal" se utilizan indistintamente. Esto se debe a que en el cálculo de rho (en el cual se basa Rholang) se utiliza el término nombre. Sin embargo, puesto que puede enviar y recibir información sobre nombres, semánticamente son como canales.

## Empezando

No existe un entorno de desarrollo integrado (IDE) para Rholang. Comience con Rholang seleccionando una de las opciones de abajo.
* __Ejecutar Rholang en RNode__ - Escriba contratos de Rholang en el editor de su elección y ejecútelos en RNode usando los modos REPL o EVAL. [Comience](https://github.com/RChain/RChain/releases) con la última versión de RNode.
* __Ejecutar Rholang en una interfaz web__ - Esta [interfaz web](http://RChain.cloud) fue creada por un miembro de la comunidad RChain.
* __Escribir Rholang usando un plugin IntelliJ__ - Este [plugin Rholang IntelliJ](https://github.com/tgrospic/Rholang-idea) fue creado por un miembro de la comunidad RChain.

## Contratos y enviando datos

    1 new HelloWorld in {
    2   contract HelloWorld(return) = {
    3     return!("Hello, World!")
    4   } |
    5   new myChannel in {
    6     HelloWorld!(*myChannel)
    7   }
    8 }

1-2) Para crear un nuevo canal privado, usamos la construcción `new ... in`. Ningún otro proceso puede enviar o recibir mensajes a través de este canal a menos que enviemos explícitamente este canal al otro proceso. Este proceso comienza creando un nuevo nombre `HelloWorld` y después ejecutando un contrato en este. La producción del 'contrato' crea un proceso que genera una copia de su cuerpo cada vez que recibe un mensaje.

3) En el canal de retorno enviamos un proceso, que es la cadena `"Hello, World!"`.

6) Enviamos el canal `myChannel` al contrato en `HelloWorld`. El operador `*` "deja de citar" un canal para obtener su proceso subyacente. En Rholang solo puede enviar procesos a través de canales; no puede enviar canales directamente a través de canales. Por lo tanto, usamos `*` para convertir el canal privado en un proceso antes de enviarlo.

## Recibiendo datos

    1 new HelloAgain in {
    2   contract HelloAgain(_) = {
    3     new chan in {
    4       chan!("Hello again, world!") |
    5       for (@text <- chan) { Nil }
    6     }
    7   } | HelloAgain!(Nil)
    8 }


2) Los contratos toman al menos un parámetro, pero podemos descartarlo vinculándolo al comodín `_`.

3) Creamos un nuevo canal `chan`.

4) Enviamos el proceso de cadena `"Hello again, world!"` por el nuevo canal.

5) Escuchamos en el nuevo canal un solo mensaje. La operación `for` se bloquea hasta que haya un mensaje disponible en el canal `chan`. En Rholang solo puede recibir nombres en los canales (¡tenga en cuenta que esto es diferente del envío!). El enlace en el lado izquierdo de `<-` en `for` es en realidad un patrón de nombre. En este ejemplo, el patrón es `@text`, lo que significa que el nombre que se recibe es un proceso citado y queremos vincular ese proceso a la variable libre `text`. La operación `for` es como un contrato, excepto que solo lee un mensaje y después se convierte en su cuerpo en lugar de bifurcar una copia de su cuerpo para cada mensaje. En este caso, elegimos no hacer nada en el cuerpo `for` simplemente haciendo que sea el proceso detenido `Nil`, sin embargo, en principio, querríamos proceder con un procesamiento adicional del `text` contenido en `chan`.

7) Iniciamos el contrato.

## Estado mutable

     1 new MakeCell in {
     2   // Genera una sola celda en la cual puede guardar valores
     3   contract MakeCell(@init, get, set) = {
     4     new valueStore in {
     5       valueStore!(init) |
     6       contract get(ack) = {
     7         for(@value <- valueStore) {
     8           valueStore!(value) | ack!(value)
     9         }
    10       } |
    11       contract set(@newValue, ack) = {
    12         for(_ <- valueStore) {
    13           valueStore!(newValue) | ack!(true)
    14         }
    15       }
    16     }
    17   } |
    18   // Uso de celda.
    19   new myGet, mySet in {
    20     MakeCell!(123, *myGet, *mySet) |
    21     new ack in {
    22       myGet!(*ack) |
    23       for (@result <- ack) {
    24         // el resultado ahora contiene el valor 123
    25         mySet!(456, *ack) |
    26         for (_ <- ack) {
    27           myGet!(*ack) |
    28           for (@result <- ack) {
    29             // el resultado ahora contiene el valor 456
    30             Nil
    31           }
    32         }
    33       }
    34     }
    35   }
    36 }

1) Creamos un nuevo canal MakeCell y después lo usamos en la línea 3 como el nombre de un contrato interno. Ningún proceso que no sea el código dentro de este ámbito léxico puede invocarlo.

3) El contrato `MakeCell` toma tres argumentos. El primer argumento es el valor inicial que será guardado en la celda. El segundo y tercer argumentos son canales en los cuales la celda recibirá solicitudes para obtener y establecer el valor. Tenga en cuenta que queremos que el primer argumento sea un proceso y el segundo y el tercero sean nombres, pero los nombres siempre se reciben por canales, por lo que debemos hacer que el primer argumento sea un patrón que comience con `@` para indicar que el nombre que recibimos como el primer argumento es un proceso citado y es ese el proceso que queremos vincular a la variable.

4) Para guardar el valor, creamos un nuevo canal. Este canal tendrá como máximo un mensaje que contenga el valor actual de la celda.

5) Antes de esta línea, no hay mensajes en el canal `valueStore`. Después de enviar el valor inicial, este es el único valor en ese canal.

6) Creamos un contrato para escuchar en el canal `get`. Cada vez que se envía un mensaje en `get`, el cuerpo del contrato se ejecutará

7) Bloqueamos hasta que recibimos un mensaje del canal `valueStore`. Debido a que hay como mucho un mensaje siempre esperando en `valueStore`, leer el mensaje se comporta de forma muy similar a la obtención de un candado (cierre de exclusión mutua).

8) Enviamos nuevamente el valor actual en `valueStore`, permitiendo que se procesen otros mensajes (es decir, retirando el cierre), y enviamos el valor actual de vuelta al cliente en el canal `ack`.

11) Al mismo tiempo que el contrato `get`, ejecutamos un contrato escuchando en `set`.

12) Bloqueamos hasta que aparezca un mensaje en `valueStore`, y entonces lo leemos. Tiramos el mensaje que leemos.

13) Enviamos el nuevo valor para guardarlo en `valueStore` e indicamos que la operación está completa.

18-36) El código de uso demuestra la creación de una celda, la asignación del valor inicial 123, la obtención de ese valor, el establecimiento de ese valor en 456, y después la obtención de ese valor.

Destacan las capas profundas de devolución de llamada (callback). Rholang fue diseñado para hacer que los cálculos concurrentes fueran expresados de forma natural. Como consecuencia, las dependencias de datos implícitas en la secuenciación en otros lenguajes deben hacerse explícitas.

## Iteración y coincidencia

En el siguiente código, mostramos un ejemplo de iteración mediante una lista.

     1	new iterate in {
     2	  contract iterate(@list, process, done) = {
     3	    match list {
     4	      [hd, ...tl] => {
     5	        new ack in {
     6	          process!(hd, *ack) |
     7	          for (_ <- ack) { iterate!(tl, *process, *done) }
     8	        }
     9	      }
    10	      _ => done!(Nil)
    11	    }
    12	  } |
    13	  new process, done in {
    14	    iterate!([4,5,6], *process, *done) |
    15	    contract process(@item, ack) = {
    16	      /* manejo del procesamiento del elemento */
    17	      ack!(Nil)
    18	    } |
    19	    for (_ <- done) {
    20	      /* listo! */
    21	      Nil
    22	    }
    23	  }
    24	}

3) La construcción `match` permite desestructurar una variable mediante la coincidencia de patrones.

4) Los patrones de la lista facilitan la coincidencia con el resto de una lista. Si `list` coincide con el patrón de un par cabeza/cola ejecutamos el cuerpo principal del ciclo.

5) Creamos un canal para que el handler de elementos nos notifique que ya está hecho con el elemento actual.

6) Llamamos al procesador en el elemento y el canal de acuse de recibo.

7) Cuando recibimos el acuse de recibo, volvemos a llamar al iterador a la cola.

10) Si la lista está vacía, indicamos que el procesamiento está completo.

14) Llamamos al iterador.

15-18) Este `contract` es llamado para cada elemento de la lista. En la línea 17, le decimos al iterador que hemos terminado con este elemento.

19) Este `for` contiene el código que debe ejecutarse cuando la iteración está completa.

## Mapas

     1 new MakeCoatCheck in {
     2   contract MakeCoatCheck(ret) = {
     3     new port, table in {
     4       ret!(*port) |
     5       for(@"new", @arg, ack <= port) {
     6         new ticket in {
     7           ack!(*ticket) |
     8           @{*ticket | *table}!(arg)
     9         }
    10       } |
    11       for(@"get", @arg, ack <= port) {
    12         for (@value <- @{arg | *table}) {
    13           @{arg | *table}!(value) |
    14           ack!(value)
    15         }
    16       } |
    17       for(@"set", @arg1, @arg2, ack <= port) {
    18         for (_ <- @{arg1 | *table}) {
    19           @{arg1 | *table}!(arg2) |
    20           ack!(true)
    21         }
    22       }
    23     }
    24   } |
    25
    26   // Uso
    27   new ret, get, set in {
    28     MakeCoatCheck!(*ret) |
    29     for (cc <- ret) {
    30       // Crea una nueva celda con valor inicial 0
    31       cc!("new", 0, *ret) |
    32       for (ticket <- ret) {
    33         contract get(return) = { cc!("get", *ticket, *return) } |
    34         contract set(@value, return) = { cc!("set", *ticket, value, *return) } |
    35         
    36         get!(*ret) | for(@r <- ret) {
    37           //r es igual a 0
    38           for(_ <- ret){
    39             set!(1, *ret) | for(_ <- ret) {
    40               get!(*ret) | for(@r <- ret) {
    41                 //r es igual a 1
    42                 Nil
    43               }
    44             }
    45           }
    46         }
    47       }
    48     }
    49   }
    50 }

2) Un patrón de diseño, utilizado en el contrato MakeCell anterior, consiste en recibir del llamador un canal para cada función diferente que proporciona un proceso. Un programador orientado a objetos podría decir que MakeCell requiere que el llamador proporcione un canal para cada método. MakeCoatCheck utiliza un enfoque más orientado a objetos, como veremos.

3) Creamos un canal `port` para interactuar con el coat check, así como un nombre de `table` que se utilizará para guardar/recuperar valores en el coat check.

4) Enviamos `port` al llamador, para que puedan interactuar con el coat check.

5, 11, 17). Definimos diferentes métodos que pueden ser llamados enviando un mensaje en `port`. Esto se hace mediante la especificación a patrones mutuamente excluyentes que el mensaje en `port` puede coincidir, siendo el primer elemento del mensaje el nombre del método y los elementos subsiguientes siendo el(los) argumento(s) y el canal de retorno. Usando la flecha `<=` en vez de la flecha `<-` significa que los `for`s son "replicados". Esto les da el mismo comportamiento que `contract`, es decir, el proceso que escucha mensajes en `port` persiste después de generar una instancia de su cuerpo.

8) Aprovechamos la posibilidad de poder citar cualquier proceso para crear un nombre con el fin de crear un nombre único en el que guardar cada valor. El proceso `*ticket|*table` es producido por la composición concurrente de los procesos producidos al dejar de citar los nombres `ticket` y `table`. Ese proceso puede citarse para formar un nombre único que luego se usa para guardar el valor enviándolo al nombre.

## Filósofos cenando e interbloqueo (deadlock)

     1 new philosopher1, philosopher2, north, south, knife, spoon in {
     2   north!(*knife) |
     3   south!(*spoon) |
     4   for (@knf <- north) { for (@spn <- south) {
     5     philosopher1!("Complete!") |
     6     north!(knf) |
     7     south!(spn)
     8   } } |
     9   for (@spn <- south) { for (@knf <- north) {
    10     philosopher2!("Complete!") |
    11     north!(knf) |
    12     south!(spn)
    13   } }
    14 }

El problema de los filósofos cenando es sobre dos filósofos que comparten un solo juego de cubiertos. El Philosopher1 se sienta en el lado este de la mesa mientras el Philosopher2 se sienta en el oeste. Cada uno necesita un cuchillo y una cuchara para comer. Cada uno se niega a renunciar a un cubierto hasta que haya usado ambos para tomar un bocado. Si ambos filósofos cogen primero el cubierto que hay a su derecha, ambos morirán de hambre: Philosopher1 coge el cuchillo, Philosopher2 coge la cuchara y ninguno suelta nunca el cubierto.

Así es cómo resolvemos el problema:

     1 new philosopher1, philosopher2, north, south, knife, spoon in {
     2   north!(*knife) |
     3   south!(*spoon) |
     4   for (@knf <- north; @spn <- south) {
     5     philosopher1!("Complete!") |
     6     north!(knf) |
     7     south!(spn)
     8   } |
     9   for (@spn <- south; @knf <- north) {
    10     philosopher2!("Complete!") |
    11     north!(knf) |
    12     south!(spn)
    13   }
    14 }

4, 9) El operador join, indicado con un punto y coma `;`, declara que la continuación solo debería continuar si hay un mensaje disponible en cada uno de los canales simultáneamente, evitando el interbloqueo anterior.

## Patrones de diseño seguros

En esta sección describimos varios patrones de diseño. Estos patrones están adaptados del [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf) de Marc Stiegler's.

### Facetas

En el contrato MakeCell, el cliente proporciona dos canales, uno para obtener el valor y otro para configurarlo. Si el cliente solo pasa el canal `get` a otro proceso, ese proceso tiene efectivamente una vista de solo lectura de la celda.

Los canales como `get` y `set` son llamados las "facetas" del proceso. Encapsulan la autoridad para realizar la acción. Si el canal `set` es un canal público como `@"Foo"`, entonces cualquier persona que pueda aprender o incluso adivinar la cadena `"Foo"` tiene la autoridad para establecer el valor de la celda. Por otro lado, si el canal `set` fue creado con el operador `new`, entonces no hay manera de que otro proceso construya el canal `set`, se debe pasar a un proceso directamente para que el proceso lo use.

Tenga en cuenta que la posesión de `get` y `set` también significa autoridad para interceptar los mensajes enviados a la celda:

    for (@ret <- get) { P } |
    for (@ret <- get) { Q } |
    get!(*ack)

Este término tiene dos procesos que escuchan en el canal `get` y un único mensaje enviado sobre `get`. Solo uno de los dos procesos podrá recibir el mensaje.

Al recibir canales del cliente para obtener y configurar, el contrato MakeCell deja al cliente tomar las decisiones sobre cuán públicos son esos canales. El contrato MakeCoatCheck, por otro lado, construye sus propios canales y expone los métodos al cliente, por lo que está en condiciones de hacer cumplir las garantías de privacidad.

### Reenviadores atenuantes

En el contrato MakeCoatCheck, solo hay un canal y los mensajes se envían internamente. Para obtener el mismo efecto que una faceta de solo lectura, podemos crear un proceso de reenviador que simplemente ignora cualquier mensaje que no desee reenviar. El contrato de abajo solo reenvía el método "get".

    new MakeGetForwarder in {
      contract MakeGetForwarder(target, ret) = {
        new port in {
          ret!(*port) |
          contract port(@method, @arg, ack) = {
            match method == "get" { true => target!("get", arg, *ack) }
          }
        }
      }
    }

### Revocación

Podemos implementar la revocación creando un reenviador con un interruptor de apagado (kill switch).

     1 new MakeRevokableForwarder in {
     2   contract MakeRevokableForwarder(target, ret) = {
     3     new port, kill, forwardFlag in {
     4       ret!(*port, *kill) |
     5       forwardFlag!(true) |
     6       contract port(msg) = {
     7         for (@status <- forwardFlag) {
     8           forwardFlag!(status) |
     9           match status { true => target!(*msg) }
    10         }
    11       } |
    12       for (_ <- kill; _ <- forwardFlag) {
    13         forwardFlag!(false)
    14       }
    15     }
    16   }
    17 }

3) Creamos un puerto para escuchar llamadas de método y un canal `forwardFlag` para guardar si reenviamos mensajes.

4) Devolvemos el canal en el que los clientes envían las solicitudes y el canal al que enviar la señal de interrupción (kill).

5) Establecemos el estado inicial de `forwardFlag` en verdadero.

6-11) Leemos en un mensaje arbitrario, obtenemos y reemplazamos el valor de flag. Si flag es verdadero, reenviamos el mensaje a `target`.

12-14) Si alguna vez se envía un mensaje en el canal `kill`, establecemos `forwardFlag` en falso. El proceso del reenviador detiene entonces el reenvío de mensajes.

### Composición

Al combinar un reenviador atenuante con un reenviador revocable, obtenemos ambas características:

    new ret in {
      MakeGetForwarder(target, ret) |
      for (@pair <- ret) {
        match pair {
          [getOnly, kill] => {
            MakeRevokableForwarder!(getOnly, *ret) |
            for (RevokableGetOnly <- ret) {
              // dar RevokableGetOnly en vez de target
              // esperar con kill para una revocación posterior
            }
          }
        }
      }
    }

### Reenviador de registro

Un reenviador de registro puede registrar todos los mensajes enviados en un canal haciendo eco de estos a un segundo canal.

    contract MakeLoggingForwarder(target, logger, ret) = {
      new port in {
        ret!(*port) |
        contract port(@msg) = {
          target!(msg) |
          logger!(msg)
        }
      }
    }

### Responsabilidad

Supongamos que Alicia tiene un canal y desea registrar el acceso de Bob a este canal. Bob desea delegar el uso de ese canal a Carol y registrar su acceso. Cada parte es libre de construir su propio reenviador de registro en el canal que ha recibido. Alice considerará responsable a Bob por cualquier cosa que haga Carol.

### Sellado y no sellado (sealling/unsealling)

    contract MakeSealerUnsealer(ret) =  {
      new sealer, unsealer, ccRet in {
        ret!(*sealer, *unsealer) |
        MakeCoatCheck!(*ccRet) |
        for (cc <- ccRet) {
          contract sealer(@value, ret) = {
            cc!("new", value, *ret)
          } |
          contract unsealer(@ticket, ret) = {
            cc!("get", ticket, *ret)
          }
        }
      }
    }


Un par sealer/unsealer ofrece la misma funcionalidad que las claves públicas, pero sin criptografía. Es simplemente una atenuación del coat check descrito anteriormente. Este patrón de diseño se puede utilizar para firmar algo en nombre del usuario. En el tutorial blockchain de Rholang, veremos que un par sealer/unsealer incluso funciona como un par de llaves de firma/verificación en el blockchain porque no hay secretos que guardar, solo nombres imposibles de descifrar que deben mantenerse inaccesibles.

### Cuidado con enviar atenuadores

Un principio básico a tener en cuenta con los procesos RChain es uno similar al de las aplicaciones web más tradicionales: cualquier código que envíe a otra parte puede ser desmontado. Desde finales de la década de 1990, cuando empezó a ser posible comprar cosas a través de internet, [ha habido plataformas de comercio electrónico](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce- security-mistakes/) en las que la plataforma dependía de los navegadores de los usuarios para devolverle el precio correcto del artículo. Los autores no pensaron en que el usuario podía abrir las herramientas de desarrollador y cambiar el precio antes de que se enviara de vuelta. La forma correcta de construir una plataforma de comercio electrónico es guardando los precios en el servidor y verificándolos allí.

Supongamos que Bob está dispuesto a ejecutar algún código para Alicia. Él tiene un contrato que dice algo así como: "Obtener un proceso de este canal y ejecutarlo".

    for (p <- x) { *p }

Esto es como un navegador web que está dispuesto a ejecutar el código JavaScript que obtiene de un sitio web. Si Alice envía a Bob un reenviador atenuante, Bob puede usar las producciones de coincidencia de patrones en Rholang para separar el proceso y obtener acceso al recurso subyacente. En cambio, al igual que en el ejemplo del comercio electrónico, Alice solo debe enviar un código que reenvía solicitudes a sus propios procesos y realizar la atenuación allí.

## Conclusión

RChain es un lenguaje diseñado para usar en un blockchain, pero no hemos mencionado nada sobre nodos, espacios de nombres, carteras, REV y phlogiston, estructura de red o Casper. Un próximo documento abordará todos estos temas y más.

Esperamos que los ejemplos anteriores despierten el deseo de escribir más código y demuestren la facilidad de expresar diseños concurrentes.
