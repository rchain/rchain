# Un tutorial su Rholang

Rholang è un nuovo linguaggio di programmazione progettato per l'uso in sistemi distribuiti. Come tutte le cose appena nate, sta crescendo e cambiando rapidamente; questo documento descrive la sintassi che verrà utilizzata nella versione 0.1 SDK.

Rholang è "orientato al processo": tutto il calcolo avviene per mezzo del passaggio dei messaggi. I messaggi vengono passati su "canali", che sono piuttosto come code di messaggi ma si comportano come set piuttosto che come code. Rholang è completamente asincrono, nel senso che mentre puoi leggere un messaggio da un canale e poi fare qualcosa con esso, non puoi inviare un messaggio e poi fare qualcosa una volta che è stato ricevuto---almeno, non senza esplicitamente in attesa di un messaggio di conferma dal ricevitore.

## Contratti e invio di dati

    1 new helloWorld in {
    2   contract helloWorld(name) = {
    3     "Hello, ".display(name, "!\n")
    4   } |
    5   helloWorld("Joe")
    6 }

1) Un programma Rholang è un processo singolo. Questo processo inizia creando un nuovo canale chiamato `helloWorld`.  Per creare un nuovo canale privato, usiamo la costruzione `new ... in`. Nessun altro processo può inviare o ricevere messaggi su questo canale a meno che non si invii esplicitamente questo canale all'altro processo.

2) La produzione `contract` crea un processo che genera una copia del suo corpo ogni volta che riceve un messaggio.

3) Il metodo `display` di una stringa scrive su standard. Prende un elenco di stringhe da stampare successivamente. Pertanto, affinché funzioni, il messaggio `name` dovrebbe essere una stringa.

5) Inviamo la stringa `"Joe"` sul canale `helloWorld`.

## Ricevimento di dati

     1 new helloAgain in {
     2   contract helloAgain(_) = {
     3     new chan in {
     4       chan("Hello again, world!") |
     5       for (text <- chan) {
     6         text.display("\n")
     7       }
     8     }
     9   } |
    10   helloAgain(Nil)
    11 }

2) I contratti prendono almeno un parametro, ma possiamo buttarlo via legandolo a una variabile che non usiamo mai.

3) Creiamo un nuovo canale `chan`.

4) Inviamo il processo di stringa `"Hello again, world!"` sul nuovo canale.

5) Ascoltiamo sul nuovo canale un singolo messaggio. L’operazione `for` si blocca fino a quando non è disponibile un messaggio sul canale `chan`. L'operazione `for` è proprio come un contratto, tranne che legge un solo messaggio e diventa poi il suo corpo, invece di creare una copia del suo corpo per ogni messaggio.

## Stato mutevole

     1 new MakeCell in {
     2   // Makes a single cell in which you can store values
     3   contract MakeCell(init, get, set) = {
     4     new valueStore in {
     5       valueStore(init) |
     6       contract get(ack) = {
     7         for(value <- valueStore) {
     8           valueStore(value) | ack(value)
     9         }
    10       } |
    11       contract set(pair) = {
    12         for(_ <- valueStore) {
    13           match pair with [newValue, ack] => {
    14             valueStore(newValue) | ack(Nil)
    15           }
    16         }
    17       }
    18     }
    19   } |
    20   // Cell usage.
    21   new myGet, mySet in {
    22     MakeCell(123, myGet, mySet) |
    23     new ack in {
    24       myGet(ack) |
    25       for (result <- ack) {
    26         result.display("\n") |
    27         mySet([456, ack]) |
    28         for (_ <- ack) {
    29           myGet(ack) |
    30           for (result <- ack) {
    31             result.display("\n")
    32           }
    33         }
    34       }
    35     }
    36   }
    37 }

1) Creiamo un nuovo canale MakeCell e quindi lo usiamo sulla riga 3 come nome di un contratto interno. Nessun processo diverso dal codice all'interno di questo ambito lessicale può richiamarlo.

3) Il contratto `MakeCell` prende tre argomenti. Il primo argomento è il valore iniziale da memorizzare nella cella. Il secondo e il terzo argomento sono i canali su cui la cella riceverà le richieste per ottenere e impostare il valore.

4) Per memorizzare il valore, creiamo un nuovo canale. Questo canale avrà al massimo un messaggio su di esso contenente il valore corrente della cella.  

5) Prima di questa riga, non ci sono messaggi sul canale `valueStore`.  Dopo aver inviato il valore iniziale, è l'unico valore su quel canale.

6) Abbiamo creato un contratto per ascoltare sul canale `get`.  Ogni volta che un messaggio viene inviato su `get`, viene eseguito il corpo del contratto.

7) Blocchiamo finché non riceviamo un messaggio dal canale `valueStore`.  Poiché al massimo c'è un solo messaggio in attesa su `valueStore`, la lettura del messaggio si comporta come l'acquisizione di un blocco.

8) Inviamo di nuovo il valore corrente su `valueStore`, consentendo l'elaborazione di altri messaggi e inviamo il valore corrente al client sul canale `ack`.

11) In parallelo con il contratto `get`, eseguiamo un contratto in ascolto su `set`.

12) Blocchiamo finché non viene visualizzato un messaggio su `valueStore`, quindi lo leggiamo. Buttiamo via il messaggio che leggiamo.

13) L'operazione `match` esegue la destrutturazione del bind, suddividendo la tupla `pair` nei suoi componenti e assegnandogli dei nomi.

14) Inviamo il nuovo valore da memorizzare su `valueStore` e segnaliamo che l'operazione è completa.

21-36) Il codice di utilizzo dimostra la creazione di una cella, assegnando il valore iniziale 123, ottenendo e stampando quel valore, impostando il valore su 456, quindi ottenendo e stampando quel valore.  

Nota i livelli profondi di callback. Rholang è stato progettato per rendere i calcoli paralleli naturali da esprimere; di conseguenza, le dipendenze dei dati implicite nel sequenziamento in altre lingue devono essere rese esplicite.

## Iterazione e corrispondenza

Nel codice qui sotto, `iterate` invia prima un canale `next` su `iterator`, e quindi per ogni messaggio ricevuto su `next` invia una coppia contenente l'elemento successivo nell'elenco e se l'iterazione è stata eseguita.

     1 new iterator, iterate in {
     2     contract iterate(list, iterator) = {
     3         new next, right in {
     4             iterator(next) |
     5             for (_ <- next) {
     6                 contract right(pair) = {
     7                     match pair with [i, limit] => {
     8                         iterator([list.nth(i), i < limit]) |
     9                         for (_ <- next) {
    10                             match i + 1 < limit with true => {
    11                                 right([i + 1, limit]) 
    12                             }
    13                         }
    14                     }
    15                 } |
    16                 right([0, list.size()])
    17             }
    18         }
    19     } |
    20     // Invoke the iterator contract on channel
    21     iterate([4,5,6], iterator) |
    22     
    23     // Interacts with the iterator
    24     for (next <- iterator) {
    25         next(Nil) |
    26         new left in {
    27             contract left(_) = {
    28                 for (pair <- iterator) {
    29                     match pair with [v, keepGoing] => {
    30                         v.display("\n") |
    31                         match keepGoing with true => { 
    32                             next(Nil) |
    33                             left(Nil) 
    34                         }
    35                     }
    36                 }
    37             } |
    38             left(Nil)
    39         }
    40     }
    41 }

7) La costruzione `match .. with` consente la destrutturazione del binding.

8) Il metodo `nth` sulle tuple consente di estrarre singoli elementi.

16) Le tuple hanno un metodo `size`.

## Maps

     1 new MakeCoatCheck in {
     2     contract MakeCoatCheck(ret) = {
     3         new port, mapStore in {
     4             mapStore(Map()) |
     5             ret(port) |
     6             contract port (method, ack, arg1, arg2) = {
     7                 match method with
     8                 "new" => {
     9                     for (map <- mapStore) {
    10                         new ticket in {
    11                             map.insert(ticket, arg1) |
    12                             mapStore(map) |
    13                             ack(ticket)
    14                         }            
    15                     }
    16                 }
    17                 "get" => {
    18                     for (map <- mapStore) {
    19                         mapStore(map) |
    20                         ack(map.get(arg1))
    21                     }
    22                 }
    23                 "set" => {
    24                     for (map <- mapStore) {
    25                         map.insert(arg1, arg2) |
    26                         mapStore(map) |
    27                         ack(Nil)
    28                     }
    29                 }
    30             }
    31         }
    32     } |
    33 
    34     // Usage
    35     new ret in {
    36         MakeCoatCheck(ret) |
    37         for (cc <- ret) {
    38             // Creates new cell with initial value 0
    39             cc("new", ret, 0, Nil) |
    40             for (ticket <- ret) {
    41                 // Sets the cell to 1
    42                 cc("set", ret, ticket, 1) |
    43                 for (ack <- ret) {
    44                     // Reads the value
    45                     cc("get", ret, ticket, Nil) |
    46                     for (storedValue <- ret) {
    47                         // Prints 1
    48                         storedValue.display("\n")
    49                     }
    50                 }
    51             }
    52         }
    53     }
    54 }

2) Uno schema di progettazione, utilizzato nel contratto MakeCell sopra, è quello di ricevere dal chiamante un canale per ogni diversa funzionalità fornita da un processo. Un programmatore orientato agli oggetti potrebbe dire che MakeCell richiede al chiamante di fornire un canale per ogni metodo. Le partite vengono tentate nell'ordine in cui appaiono nel codice; se non si verifica alcuna corrispondenza, il blocco `match` valuta il processo `Nil`.  MakeCoatCheck utilizza un approccio più orientato agli oggetti, come vedremo.

3-4) Ogni controllo di cappotto ha la propria mappa rientranti mutevole in cui conservare gli oggetti. Memorizziamo la mappa appena costruita su mapStore. Ha la seguente API:

    insert(key, value)
    insertMany(key1, val1, key2, val2, ..., keyn, valn)
    getOrElse(key, default)
    get(key)

6) Ci aspettiamo quattro argomenti ogni volta; potevamo anche aspettarci una singola tupla e usare il binding destructuring per la spedizione in base al metodo e alla lunghezza della tupla.

## Filosofi a cena e stallo

     1 new north, south, knife, spoon in {
     2     north(knife) |
     3     south(spoon) |
     4     for (knf <- north) { for (spn <- south) {
     5         "Philosopher 1 Utensils: ".display(knf, ", ", spn, "\n") |
     6         north(knf) |
     7         south(spn)
     8     } } |
     9     for (knf <- north) { for (spn <- south) {
    10         "Philosopher 2 Utensils: ".display(knf, ", ", spn, "\n") |
    11         north(knf) |
    12         south(spn)
    13     } }
    14 }

Il problema dei filosofi a cena vede due filosofi condividere un solo set di posate. Philosopher1 siede sul lato est del tavolo, mentre Philosopher2 si trova a ovest. Ognuno ha bisogno sia di un coltello che di una forchetta per mangiare. Ognuno si rifiuta di rinunciare a un utensile finché non li ha usati entrambi per prendere un boccone. Se entrambi i filosofi raggiungono per primo l'utensile alla loro destra, entrambi moriranno di fame: Philosopher1 prende il coltello, Philosopher2 prende la forchetta e nessuno dei due lascerà mai andare la propria posata.

Ecco come risolvere il problema:

     1 new north, south, knife, spoon in {
     2     north(knife) |
     3     south(spoon) |
     4     for (knf <- north; spn <- south) {
     5         "Philosopher 1 Utensils: ".display(knf, ", ", spn, "\n") |
     6         north(knf) |
     7         south(spn)
     8     } |
     9     for (spn <- south; knf <- north) {
    10         "Philosopher 2 Utensils: ".display(knf, ", ", spn, "\n") |
    11         north(knf) |
    12         south(spn)
    13     }
    14 }

4, 9) L'operatore join, indicato con un punto e virgola `;`, dichiara che la continuazione dovrebbe procedere solo se c'è un messaggio disponibile su ciascuno dei canali simultaneamente, impedendo il deadlock sopra descritto.

## Modelli di design sicuri

In questa sezione descriviamo diversi modelli di design. Questi schemi sono adattati di Marc Stiegler [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf).

### Facet

Nel contratto MakeCell, il client fornisce due canali, uno per ottenere il valore e uno per impostarlo. Se il client passa quindi solo il canale `get` a un altro processo, quel processo ha effettivamente una vista di sola lettura della cella.  

Canali come `get` e `set` sono chiamati "facce" del processo. Incapsulano l'autorità per eseguire l'azione. Se il canale `set` è un canale pubblico come `@"Foo"`, allora chiunque possa imparare o persino indovinare la stringa `"Foo"` ha l'autorità per impostare il valore della cella. D'altra parte, se il canale `set` è stato creato con l'operatore `new`, allora non c'è modo per nessun altro processo di costruire il canale `set`; deve essere passato a un processo direttamente affinché il processo lo usi.  

Nota che se `get` e `set` non vengono creati come metà di iopairs, allora il possesso di quei canali è anche l'autorità per intercettare i messaggi inviati alla cella:

    for (ret <- get) { P } | 
    for (ret <- get) { Q } | 
    get(ack)

Questo termine ha due processi in ascolto sul canale `get` e un singolo messaggio inviato su `get`.  Solo uno dei due processi sarà in grado di ricevere il messaggio.

Ricevendo i canali dal client per ricevere e impostare, il contratto MakeCell lascia le decisioni su come quanto questi canali siano pubblici per il cliente. Il contratto MakeCellFactory, d'altra parte, costruisce i propri canali e li restituisce al cliente, quindi è in grado di far rispettare le garanzie sulla privacy.

### Attenuazione degli forwarders

Nel contratto MakeCellFactory, c'è un solo canale e i messaggi vengono inviati internamente. Per ottenere lo stesso effetto di un aspetto di sola lettura, possiamo creare un processo di inoltro che semplicemente ignora tutti i messaggi che non desidera inoltrare. Il contratto sotto inoltra solo il metodo "ottieni".

    contract MakeGetForwarder(target, ret) = {
        new port in {
            ret(port) |
            contract port(tuple) = {
                tuple.nth(0) match with "get" => target(tuple)
            }
        }
    }

### Revoca

Possiamo implementare la revoca creando un forwarder con un kill switch.

     1 contract MakeRevokableForwarder(target, ret) = {
     2     new port, kill, forwardFlag in {
     3         ret(port, kill) |
     4         forwardFlag(true) |
     5         contract port(tuple) = {
     6             for (status <- forwardFlag) {
     7                 forwardFlag(status) |
     8                 match status with true => { target(tuple) }
     9             }
    10         } |
    11         for (_ <- kill; _ <- forwardFlag) {
    12             forwardFlag(false)
    13         }
    14     }
    15 }

2) Creiamo una porta per ascoltare le chiamate ai metodi e un canale `forwardFlag` per memorizzare se inoltrare i messaggi.

3) Restituiamo il canale su cui i client inviano richieste e il canale su cui inviare il segnale kill.

4) Impostiamo lo stato iniziale di `forwardFlag` su true.

5-10) Leggiamo in una tupla arbitraria di parti di messaggi e otteniamo e sostituiamo il valore del flag. Se il flag è true, inoltriamo la tupla del messaggio a `target`.

11-13) Se un messaggio viene mai inviato sul canale `kill`, impostiamo `forwardFlag` su false.  Il processo di inoltro interrompe quindi l'inoltro dei messaggi.

### Composizione

Combinando un forwarder attenuante con un forwarder revocabile, otteniamo entrambe le funzionalità:

    new ret in {
        MakeGetForwarder(target, ret) |
        for (pair <- ret) {
            match pair with [getOnly, kill] => {
                MakeRevokableForwarder(getOnly, ret) |
                for (revokableGetOnly <- ret) {
                    // give away revokableGetOnly instead of target
                    // hang onto kill for later revocation
                }
            }
        }
    }

### Logger di inoltro

Un inoltro di log può registrare tutti i messaggi inviati su un canale facendoli eco su un secondo canale.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new port in {
            ret(port) |
            contract port(tuple) {
                target(tuple) |
                logger(tuple)
            }
        }
    }

### Responsabilità

Supponi che Alice abbia un canale e desideri registrare l'accesso di Bob ad esso. Bob vorrebbe delegare l'uso di quel canale a Carol e registrare il suo accesso. Ciascuna delle parti è libera di costruire il proprio server di inoltro del registro attorno al canale che ha ricevuto. Alice riterrà Bob responsabile per qualsiasi cosa Carol faccia.

### Sigillare e togliere il sigillo

    contract MakeSealerUnsealer(ret) =  {
        new sealer, unsealer, ccRet in {
            ret(sealer, unsealer) |
            MakeCoatCheck(ccRet) |
            for (cc <- ccRet) {
                contract sealer(value, ret) = {
                    cc("new", ret, value, Nil)
                } |
                contract unsealer(ticket, ret) = {
                    cc("get", ret, ticket, Nil)
                }
            }
        }
    }


Una coppia sealer/unsealer fornisce la stessa funzionalità delle chiavi pubbliche, ma senza crittografia. È semplicemente un'attenuazione del controllo del cappotto descritto sopra. Questo modello di progettazione può essere utilizzato per firmare qualcosa per conto di un utente. Nel tutorial sulla blockchain di Rholang, vedremo che funziona anche sulla blockchain perché non ci sono segreti da archiviare, solo i nomi che non possono essere conservati sono inaccessibili.

### Attenzione all'invio di attenuatori

Un principio di base da tenere a mente con i processi RChain è simile a quello delle applicazioni web più tradizionali: qualsiasi codice inviato a un'altra parte può essere disassemblato. Fin dalla fine degli anni '90, quando è diventato possibile acquistare cose sul web, [ci sono state piattaforme di e-commerce](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/) in cui la piattaforma si è basata sui browser degli utenti per inviare di nuovo il prezzo corretto dell'articolo. Gli autori non pensavano che l'utente avrebbe aperto gli strumenti di sviluppo e modificato il prezzo prima di essere rispedito. Il modo giusto per costruire una piattaforma di e-commerce è quello di memorizzare i prezzi sul server e controllarli lì.

Supponiamo che Bob sia disposto a eseguire codice per Alice; ha un contratto che dice qualcosa come "Prendi un processo da questo canale ed eseguilo".

    for (p <- x) { *p }

Questo è come se un browser web fosse disposto a eseguire il codice JavaScript che ottiene da un sito web. Se Alice invia a Bob un forwarder attenuante, Bob può utilizzare le produzioni di pattern matching in Rholang per smantellare il processo e ottenere l'accesso alla risorsa sottostante. Invece, come nell'esempio di e-commerce, Alice dovrebbe inviare solo il codice che inoltra richieste ai suoi processi e ne esegue l'attenuazione.

## Conclusione

RChain è un linguaggio progettato per l'uso su una blockchain, ma non abbiamo menzionato nulla su nodi, namespace, portafogli, Rev e phlogiston, struttura di rete o Casper. Un prossimo documento affronterà tutti questi problemi e altro ancora.

Speriamo che i precedenti esempi suscitino il desiderio di scrivere più codice e dimostrare la facilità di esprimere progetti concomitanti.
