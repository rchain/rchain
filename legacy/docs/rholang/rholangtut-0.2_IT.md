# Un tutorial di Rholang

Rholang è un nuovo linguaggio di programmazione progettato per l'uso nei sistemi distribuiti. Come tutte le cose appena nate, sta crescendo e sta cambiando rapidamente; questo documento descrive la sintassi che verrà utilizzata nella versione RNode-0.2.

Rholang è "orientato al processo": tutto il calcolo avviene per mezzo del passaggio dei messaggi. I messaggi vengono passati su "canali", che sono piuttosto come delle code di messaggi ma che si comportano come set piuttosto che come code. Rholang è completamente asincrono, nel senso che mentre si può leggere un messaggio da un canale e poi fare qualcosa con esso, non si può inviare un messaggio e poi fare qualcosa una volta che questo è stato ricevuto---almeno, non senza aver esplicitamente atteso un messaggio di conferma dal ricevitore. Si noti che in tutto questo documento le parole "nome" e "canale" sono usate in modo intercambiabile. Questo perché nel rho-calculus (su cui è basato Rholang) viene usato il termine nome, tuttavia poiché è possibile inviare e ricevere informazioni sui nomi, semanticamente i nomi sono come i canali.

## Contratti e invio di dati

    1 contract @"HelloWorld"(return) = {
    2   return!("Hello, World!")
    3 } |
    4 new myChannel in {
    5   @"HelloWorld"!(*myChannel)
    6 }

1) Un programma Rholang è un processo singolo con una o più composizioni simultanee. Questo processo inizia creando un contratto con il nome `@"HelloWorld"`. La produzione del `contract` crea un processo che genera una copia del suo corpo ogni volta che riceve un messaggio. Si noti che in Rholang tutti i processi possono essere "quotati" con `@` per creare un canale. Le stringhe sono solo processi speciali, quindi possiamo citare qualsiasi stringa per produrre un canale.

2) Sul canale di ritorno inviamo un processo, che è la stringa `"Hello, World!"`.

4) Per creare un nuovo canale privato, usiamo la costruzione `new ... in`. Nessun altro processo può inviare o ricevere messaggi su questo canale a meno che non si invii esplicitamente questo canale all'altro processo.

5) Inviamo il canale `myChannel` al contratto a`@"HelloWorld"`. L'operatore `*` "svela" un canale per ottenere il suo processo sottostante. In Rholang si possono inviare solo processi su canali; non è possibile inviare canali direttamente sui canali. Pertanto, usiamo `*` per trasformare il canale privato in un processo prima dell'invio.

## Ricevere dati

    1 contract @"HelloAgain"(_) = {
    2   new chan in {
    3     chan!("Hello again, world!") |
    4     for (@text <- chan) { Nil }
    5   }
    6 } | @"HelloAgain"!(Nil)

1) I contratti prendono almeno un parametro, ma noi possiamo liberarcene legandolo al carattere jolly `_`.

2) Creiamo un nuovo canale `chan`.

3) Inviamo il processo di stringa `"Hello again, world!"` Sul nuovo canale.

4) Rimaniamo in ascolto sul nuovo canale per un messaggio singolo.  L'operazione `for` si blocca fino a quando non è disponibile un messaggio sul canale `chan`. In Rholang si possono solo ricevere nomi sui canali (si noti che questo differisce dall'invio!). L'associazione sul lato sinistro di `<-` nel `for` è in realtà un pattern di nome. In questo esempio il pattern è `@text`, il che significa che il nome ricevuto è un processo quotato e vogliamo associare quel processo alla variabile libera `text`. L'operazione `for` è proprio come un contratto, tranne il fatto che legge un solo messaggio e diventa il suo corpo invece di creare una copia del suo corpo per ogni messaggio. In questo caso scegliamo di non fare nulla nel corpo `for` rendendolo semplicemente processo arrestato `Nil`, tuttavia in linea di principio vorremmo procedere con qualche ulteriore elaborazione del `text` contenuto in `chan`.

## Stato mutevole

     1 new MakeCell in {
     2   // Makes a single cell in which you can store values
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
    18   // Cell usage.
    19   new myGet, mySet in {
    20     MakeCell!(123, *myGet, *mySet) |
    21     new ack in {
    22       myGet!(*ack) |
    23       for (@result <- ack) {
    24         //result now contains the value 123
    25         mySet!(456, *ack) |
    26         for (_ <- ack) {
    27           myGet!(*ack) |
    28           for (@result <- ack) {
    29             //result now contains the value 456
    30             Nil
    31           }
    32         }
    33       }
    34     }
    35   }
    36 }

1) Creiamo un nuovo canale MakeCell e quindi lo usiamo sulla riga 3 come nome di un contratto interno. Nessun processo diverso dal codice all'interno di questo ambito lessicale può richiamarlo.

3) Il contratto `MakeCell` prende tre argomenti. Il primo argomento è il valore iniziale da memorizzare nella cella. Il secondo e il terzo argomento sono i canali su cui la cella riceverà le richieste per ottenere e impostare il valore. Si noti che vogliamo che il primo argomento sia un processo e che il secondo e il terzo siano nomi, ma i nomi vengono sempre ricevuti sui canali, quindi dobbiamo rendere il primo argomento un pattern che inizia con `@` per indicare che il nome che riceviamo come primo argomento è un processo quotato ed è quello il processo che vogliamo associare alla variabile.

4) Per memorizzare il valore, creiamo un nuovo canale. Questo canale avrà al massimo un messaggio su di esso contenente il valore corrente della cella.

5) Prima di questa riga, non ci sono messaggi sul canale `valueStore`. Dopo aver inviato il valore iniziale, sarà l'unico valore su quel canale.

6) Abbiamo creato un contratto per rimanere in ascolto sul canale `get`. Ogni volta che un messaggio viene inviato su `get`, viene eseguito il corpo del contratto

7) Blocchiamo finché non riceviamo un messaggio dal canale `valueStore`. Poiché al massimo c'è un solo messaggio in attesa su `valueStore`, la lettura del messaggio si comporta come l'acquisizione di un lock.

8) Inviamo di nuovo il valore corrente su `valueStore`, consentendo l'elaborazione di altri messaggi (cioè rilasciando il lock) e inviamo il valore corrente al client sul canale `ack`.

11) In concomitanza con il contratto `get`, eseguiamo un contratto in ascolto su` set`.

12) Blocchiamo finché non viene visualizzato un messaggio su `valueStore`, quindi lo leggiamo. Buttiamo via il messaggio che abbiamo letto.

13) Inviamo il nuovo valore da memorizzare su `valueStore` e segnaliamo che l'operazione è completa.

18-36) Il codice di utilizzo dimostra la creazione di una cella, l'assegnazione del valore iniziale 123, l'ottenimento di quel valore, l'impostazione del valore su 456, quindi l'ottenimento di questo valore.

Si notino i livelli profondi di callback. Rholang è stato progettato per rendere i calcoli simultanei naturali da esprimere; di conseguenza, le dipendenze dei dati implicite nel sequenziamento in altre lingue devono essere rese esplicite.

## Iterazione e corrispondenza

Nel codice sottostante, mostriamo un esempio di iterazione attraverso un elenco collegato, implementato come coppie nidificate [testa, coda].

     1 new chan, loop, iCh in {
     2   contract loop(@list, @acc, return) = {
     3     match list { 
     4       [hd, tl] => {
     5         for(@i <- iCh) {
     6           iCh!(i + 1) |
     7           match [hd == i, acc] {
     8             [true, true] => { loop!(tl, true, *return) }
     9             _ => { loop!(tl, false, *return) }
    10           }
    11         }
    12       }
    13       _ => { return!(acc) }
    14     }
    15   } |
    16   iCh!(1) |
    17   loop!([1, [2, [3, [4, []]]]], true, *chan)
    18 }

3) La costruzione `match` consente di destrutturare una variabile attraverso la corrispondenza del pattern.

4) Se `list` corrisponde al pattern di una coppia testa/coda, eseguiamo il corpo principale del ciclo.

5) Leggiamo l'indice corrente dal canale.

6) Aumentiamo l'indice e lo memorizziamo.

7) La parola chiave `match` consente anche di effettuare una corrispondenza tra espressioni più complesse e i pattern.

8) Qui confermiamo che la testa della lista è uguale all'indice corrente e che l'accumulatore è ancora uguale a `true` e richiama ancora ricorsivamente il ciclo.

13) Se `list` non corrisponde al pattern della coppia testa/coda, il ciclo è finito e restituiamo il valore accumulato.

16) Inizializza il canale dell'indice.

17) Chiama il ciclo con la nostra lista iniziale.

## Mappe

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
    26   // Usage
    27   new ret, get, set in {
    28     MakeCoatCheck!(*ret) |
    29     for (cc <- ret) {
    30       // Creates new cell with initial value 0
    31       cc!("new", 0, *ret) |
    32       for (ticket <- ret) {
    33         contract get(return) = { cc!("get", *ticket, *return) } |
    34         contract set(@value, return) = { cc!("set", *ticket, value, *return) } |
    35         
    36         get!(*ret) | for(@r <- ret) {
    37           //r is equal to 0
    38           set!(1, *ret) | for(_ <- ret) {
    39             get!(*ret) | for(@r <- ret) {
    40               //r is equal to 1
    41               Nil
    42             }
    43           }
    44         }
    45       }
    46     }
    47   }
    48 }

2) Uno schema di progettazione, utilizzato nel contratto MakeCell di cui sopra, è quello di ricevere dal chiamante un canale per ogni diversa funzionalità fornita da un processo. Un programmatore orientato agli oggetti potrebbe dire che MakeCell richiede al chiamante di fornire un canale per ogni metodo. MakeCoatCheck utilizza un approccio più orientato agli oggetti, come vedremo.

3) Creiamo un canale `port` per interagire con il "coatcheck" e un nome `table` che sarà usato per memorizzare/recuperare i valori nel "coatcheck".

4) Inviamo `port` al chiamante, in modo che possano interagire con il "coatcheck".

5, 11, 17) Definiamo diversi metodi che possono essere chiamati inviando un messaggio su `port`. Questo viene fatto specificando pattern mutualmente esclusivi a cui il messaggio su `port` può corrispondere, con il nome del metodo come primo elemento del messaggio e l'argomento(i) e il canale di ritorno come elementi successivi. Usando la freccia `<=` invece della freccia `<-` significa che i `for` sono "replicati". Questo dà loro lo stesso comportamento di `contract`, cioè il processo in ascolto dei messaggi su `port` persiste dopo aver generato un'istanza del suo corpo.

8) Approfittiamo della possibilità di citare qualsiasi processo per creare un nome al fine di creare un nome univoco per ogni valore presso cui memorizzare. Il processo `*ticket | *table` è prodotto dalla simultanea composizione dei processi prodotti dallo smantellamento dei nomi `ticket` e` table`. Tale processo può quindi essere quotato per formare un nome univoco che viene quindi utilizzato per memorizzare il valore inviandolo sul nome.

## I filosofi a cena e lo stallo

     1 new philosopher1, philosopher2, north, south, knife, spoon in {
     2     north!(*knife) |
     3     south(!*spoon) |
     4     for (@knf <- north) { for (@spn <- south) {
     5         philosopher1!("Complete!") |
     6         north!(knf) |
     7         south!(spn)
     8     } } |
     9     for (@spn <- south) { for (@knf <- north) {
    10         philosopher2!("Complete!") |
    11         north(knf) |
    12         south(spn)
    13     } }
    14 }

Il problema dei filosofi a cena vede due filosofi che condividono solo un set di posate. Il Filosofo1 siede sul lato est del tavolo mentre il Filosofo2 si trova a ovest. Ognuno ha bisogno sia di un coltello che di un cucchiaio per poter mangiare. Ognuno si rifiuta di rinunciare a un utensile finché non ha usato entrambi per prendere un boccone. Se entrambi i filosofi raggiungono per primi l'utensile alla loro destra, entrambi moriranno di fame: Il Filosofo1 prende il coltello, il Filosofo2 prende il cucchiaio e nessuno dei due li lascia mai andare.

Ecco come risolvere il problema:

     1 new philosopher1, philosopher2, north, south, knife, spoon in {
     2     north!(*knife) |
     3     south!(*spoon) |
     4     for (@knf <- north; @spn <- south) {
     5         philosopher1!("Complete!") |
     6         north!(knf) |
     7         south!(spn)
     8     } |
     9     for (@spn <- south; @knf <- north) {
    10         philosopher2!("Complete!") |
    11         north!(knf) |
    12         south!(spn)
    13     }
    14 }

4, 9) L'operatore di unione, indicato con un punto e virgola `;`, dichiara che la continuazione dovrebbe procedere solo se c'è un messaggio disponibile su ciascuno dei canali simultaneamente, evitando lo stallo di cui sopra.

## Modelli di progettazione sicuri

In questa sezione descriviamo diversi modelli di progettazione. Questi modelli sono adattati dal [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf) di Marc Stiegler.

### Sfaccettature

Nel contratto MakeCell, il client fornisce due canali, uno per ottenere il valore e uno per impostarlo. Se il client passa quindi solo il canale `get` a un altro processo, quel processo ha effettivamente una vista della cella che è di sola lettura.

I canali come `get` e `set` sono chiamati "sfaccettature" del processo. Incapsulano l'autorità per eseguire l'azione. Se il canale `set` è un canale pubblico come `@"Foo"`, allora chiunque possa imparare o persino indovinare la stringa`"Foo"`ha l'autorità per impostare il valore della cella. D'altra parte, se il canale `set` è stato creato con l'operatore `new`, allora non c'è modo per nessun altro processo di costruire il canale `set`; deve essere passato a un processo direttamente affinché il processo lo usi.

Si noti che il possesso di `get` e `set` da anche l'autorità per intercettare i messaggi inviati alla cella:

    for (@ret <- get) { P } | 
    for (@ret <- get) { Q } | 
    get!(*ack)

Questo termine ha due processi in ascolto sul canale `get` e un singolo messaggio inviato su `get`. Solo uno dei due processi sarà in grado di ricevere il messaggio.

Ricevendo i canali dal client per ottenere e impostare, il contratto MakeCell lascia al cliente la decisione su come questi canali siano pubblici. Il contratto MakeCoatCheck, d'altra parte, costruisce i propri canali e espone i metodi al cliente, quindi è in grado di far rispettare le garanzie sulla privacy.

### Attenuazione dei mittenti

Nel contratto MakeCoatCheck, c'è un solo canale e i messaggi vengono inviati internamente. Per ottenere lo stesso effetto di un aspetto di sola lettura, possiamo creare un processo di inoltro che semplicemente ignora tutti i messaggi che non desidera inoltrare. Il contratto sottostante inoltra solo il metodo "get".

    contract MakeGetForwarder(target, ret) = {
        new port in {
            ret!(*port) |
            contract port(method, @arg, ack) = {
                method == "get" match { true => target!("get", arg, *ack) }
            }
        }
    }

### Revoca

Possiamo implementare la revoca creando un forwarder con un sistema di difesa (kill switch).

     1 contract MakeRevokableForwarder(target, ret) = {
     2     new port, kill, forwardFlag in {
     3         ret!(*port, *kill) |
     4         forwardFlag!(true) |
     5         contract port(msg) = {
     6             for (@status <- forwardFlag) {
     7                 forwardFlag!(status) |
     8                 status match { true => target!(msg) }
     9             }
    10         } |
    11         for (_ <- kill; _ <- forwardFlag) {
    12             forwardFlag!(false)
    13         }
    14     }
    15 }

2) Creiamo una porta per ascoltare le chiamate ai metodi e un canale `forwardFlag` per memorizzare se inoltrare i messaggi.

3) Restituiamo il canale su cui i client inviano richieste e il canale su cui inviare il segnale kill.

4) Impostiamo lo stato iniziale di `forwardFlag` su true.

5-10) Leggiamo in un messaggio arbitrario get e sostituiamo il valore del flag. Se il flag è true, inoltriamo il messaggio a `target`.

11-13) Se un messaggio viene mai inviato sul canale `kill`, impostiamo `forwardFlag` su false. Il processo di inoltro interrompe quindi l'inoltro dei messaggi.

### Composizione

Combinando un forwarder attenuante con un forwarder revocabile, otteniamo entrambe le funzionalità:

    new ret in {
        MakeGetForwarder(target, ret) |
        for (@pair <- ret) {
            pair match [getOnly, kill] => {
                MakeRevokableForwarder!(getOnly, *ret) |
                for (revokableGetOnly <- ret) {
                    // give away revokableGetOnly instead of target
                    // hang onto kill for later revocation
                }
            }
        }
    }

### Forwarder registrato

Un forwarder registrato può registrare tutti i messaggi inviati su un canale facendogli eco su un secondo canale.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new port in {
            ret!(*port) |
            contract port(msg) = {
                target!(msg) |
                logger!(msg)
            }
        }
    }

### Responsabilità

Supponi che Alice abbia un canale e desideri registrare l'accesso di Bob ad esso. Bob vorrebbe delegare l'uso di quel canale a Carol e registrare il suo accesso. Ciascuna delle parti è libera di costruire il proprio forwarder registrato sul canale che ha ricevuto. Alice riterrà Bob responsabile per qualsiasi cosa Carol faccia.

### Sigillare e disigillare

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


Una coppia sigillante/disigillante fornisce la stessa funzionalità delle chiavi pubbliche, ma senza crittografia. È semplicemente un'attenuazione del "coatcheck" descritto sopra. Questo pattern di progettazione può essere utilizzato per firmare qualcosa per conto di un utente. Nel tutorial sulla blockchain di Rholang, vedremo che una coppia sigillante/disigillante funziona anche come coppia di firma/verifica sulle chiavi della blockchain perché non ci sono segreti da memorizzare, solo nomi immodificabili da mantenere inaccessibili.

### Attenzione all'invio di attenuatori

Un principio di base da tenere a mente con i processi di BCot è simile a quello delle applicazioni web più tradizionali: qualsiasi codice inviato a un'altra parte può essere disassemblato. Fin dalla fine degli anni '90, quando è diventato possibile acquistare beni sul web, [ci sono state piattaforme di e-commerce](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/) in cui la piattaforma si basava sui browser degli utenti per inviare di nuovo il prezzo corretto dell'articolo. Gli autori non hanno pensato al fatto che l'utente avrebbe potuto aprire gli strumenti degli sviluppatori per modificare il prezzo prima che fosse rispedito. Il modo corretto per costruire una piattaforma di e-commerce è quello di memorizzare i prezzi sul server e di controllarli da lì.

Supponiamo che Bob sia disposto a eseguire un codice per Alice; ha un contratto che dice qualcosa come "Prendi un processo da questo canale ed eseguilo".

    for (p <- x) { *p }

Questo è come se un browser web fosse disposto a eseguire il codice JavaScript che ottiene da un sito web. Se Alice invia a Bob un forwarder attenuante, Bob può utilizzare le produzioni di corrispondenza di pattern in Rholang per smantellare il processo e ottenere l'accesso alla risorsa sottostante. Invece, come nell'esempio di e-commerce, Alice dovrebbe inviare solo il codice che inoltra richieste ai suoi processi ed effettuare li l'attenuazione.

## Conclusione

RChain è un linguaggio progettato per l'uso su una blockchain, ma non abbiamo menzionato nulla su nodi, namespace, portafogli, Rev e phlogiston, strutture di rete o Casper. Un prossimo documento affronterà tutte queste questioni e altro ancora.

Speriamo che i precedenti esempi suscitino il desiderio di scrivere più codice e di dimostrare la facilità di esprimere progetti simultanei.

