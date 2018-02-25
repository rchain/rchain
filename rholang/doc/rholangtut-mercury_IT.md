# Un tutorial su Rholang

Rholang è un nuovo linguaggio di programmazione progettato per l'uso in sistemi distribuiti. Come tutte le cose appena nate, sta crescendo e cambiando rapidamente; questo documento descrive la sintassi che verrà utilizzata nella versione Mercury.

Rholang è "orientato al processo": tutto il calcolo avviene per mezzo del passaggio dei messaggi. I messaggi vengono passati su "canali", che sono un po’ come code di messaggi, ma si comportano come set piuttosto che come code. Rholang è completamente asincrono, nel senso che mentre puoi leggere un messaggio da un canale e poi farne qualcosa, non puoi inviare un messaggio e poi fare qualcosa una volta che è stato ricevuto---almeno, non senza aspettare esplicitamente un messaggio di conferma dal ricevitore.

## Contratti, riflessioni e invio di dati

    1 contract @"HelloWorld"(system) = {
    2     system!("print", "Hello, world!")
    3 }

1) Su Internet, i server hanno degli indirizzi IP. Il Domain Name Service (DNS) mappa le stringhe alfanumeriche in numeri, come in una rubrica telefonica. Invece di usare numeri o stringhe, Rholang è "riflettente": tutti i canali sono nominati da un processo serializzato. Tutte le serializzazioni dei processi iniziano con un `@`.  Questo contratto ascolta i messaggi inviati sul canale chiamato dalla serializzazione del processo di stringa `"HelloWorld"`.  Per essere concisi, diciamo, "Questo contratto ascolta il nome `@"HelloWorld"`."

Un contratto dichiara un'API tramite la quale altri processi possono interagire con esso. I contratti di livello superiore hanno tutti la stessa API: hanno un argomento, il canale per il processo `system`.  Il processo `system` contiene i nomi di tutti i canali che possono causare effetti collaterali sul nodo. È un processo integrato che ascolta i messaggi costituiti da un nome di metodo ed alcuni argomenti.

2) L'operatore del punto esclamativo invia il messaggio alla sua destra sul canale alla sua sinistra. Ogni messaggio è una tupla di nomi. Se inviamo un processo anziché un nome, esso viene automaticamente serializzato in un nome.

In questo caso, inviamo un messaggio costituito da due processi, la stringa `"print"` e la stringa `"Hello, world!"`.  Il processo `system` è un processo integrato che ascolta i messaggi costituiti da un nome di metodo e da alcuni argomenti; in questo caso, il processo richiama il secondo argomento sullo standard output.

## Nuovi canali, ricezione di dati e modelli

    1 contract @"HelloAgain"(system) = new chan in {
    2     chan!("Hello again, world!") |
    3     for (@text <- chan) system!("print", text)
    4 }

1) Per creare un nuovo canale privato, usiamo la costruzione `new ... in`. Nessun altro processo può inviare o ricevere messaggi su questo canale a meno che non si invii esplicitamente questo canale all'altro processo.

2) Inviamo il processo di stringa `"Hello again, world!"` sul nuovo canale.

3) Ascoltiamo un messaggio sul nuovo canale. I blocchi operativi `for` fino a quando non è disponibile un messaggio sul canale `chan`. 

Il linguaggio dei modelli (noto anche come "tipi spaziali") include il linguaggio dei processi e dei nomi. Ogni processo con variabili libere è un modello che corrisponde a un processo con la stessa struttura; le variabili libere si legano ai sottoprocessi in quella posizione. Alla riga 3, ci aspettiamo una tupla con un elemento. Quell'elemento è la serializzazione di qualche processo, e quindi inizia con un `@`.  Poiché abbiamo inviato una stringa sulla riga 2, la variabile libera`text` viene associata a quel processo stringa. Infine, la riga 3 inoltra quella stringa al processo `system` da stampare.

Più avanti, esploreremo alcune altre caratteristiche dei modelli.

## Stato mutevole, replica e selezione

     1 contract @"CellDemo"(system) = new MakeCell in {
     2     // Makes a single cell in which you can store values
     3     contract MakeCell(@init, get, set) = new valueStore in {
     4         valueStore!(init) |
     5         for (@value <= valueStore) {
     6             select {
     7                 ack <- get => valueStore!(value) | ack!(value)
     8                 @newValue, ack <- set => valueStore!(newValue) | ack!()
     9             }
    10         }
    11     } |
    12 
    13     // Cell usage.
    14     new myGet, mySet in {
    15         MakeCell(123, *myGet, *mySet) |
    16         new ack in {
    17             myGet!(*ack) |
    18             for (@result <- ack) {
    19                 system!("print", result, *ack) |
    20                 for (_ <- ack) {
    21                     mySet!(456, *ack) |
    22                     for (_ <- ack) {
    23                         myGet!(*ack) |
    24                         for (@result <- ack) {
    25                             system!("print", result)
    26                         }
    27                     }
    28                 }
    29             }
    30         }
    31     }
    32 }

1) Creiamo un nuovo canale MakeCell, e quindi lo usiamo sulla riga 3 come nome di un contratto interno. Nessun processo diverso dal codice all'interno del contratto `@"CellDemo"` può invocarlo.

3) Il contratto `MakeCell` prende tre argomenti. Il primo argomento è un processo, il valore iniziale da memorizzare nella cella. Il secondo e il terzo argomento sono i canali su cui la cella riceverà le richieste per ottenere e impostare il valore. Chi ha familiarità con C++ può pensare ai canali come l’equivalente approssimativo dei puntatori, nel senso che un puntatore è un tipo di dati serializzabile che denota una posizione. L'operatore `@` in questo contesto è approssimativamente equivalente a `&` che indica il riferimento pass-by; la variabile `init` viene associata a un processo anziché a un canale. In C++, per convertire un puntatore in un riferimento, si usa l'operatore `*`; allo stesso modo, in Rholang per convertire un canale in un processo, usiamo l'operatore `*`.  Dato che possiamo inviare processi solo su un canale, l'operatore `*` viene usato molto spesso; vedi le righe dispari nell'esempio di utilizzo.

Per memorizzare il valore, creiamo un nuovo canale. Questo canale avrà al massimo un messaggio su di esso, contenente il valore corrente della cella.

4) Prima di questa riga, non ci sono messaggi sul canale `valueStore`.  Dopo aver inviato il valore iniziale, è l'unico valore su quel canale.

5) In parallelo con 4, proviamo a leggere da `valueStore`.  La freccia a doppio stelo dice che una volta ricevuto un messaggio su quel canale, dovremmo generare una copia del processo immediatamente dopo il `for` e iniziare subito a cercare di leggere su quel canale.

Una volta che un messaggio diventa disponibile, leghiamo la variabile `value` al processo nel messaggio.

6-8) La parola chiave `select` sulla riga 6 significa che solo uno dei rami sulle linee 7 o 8 sarà in grado di procedere. A questo punto, non ci sono messaggi in attesa sul canale `valueStore`.

Se c'è un messaggio sul canale `get`, allora il ramo sulla linea 7 può essere eseguito. La variabile `ack` viene legata al messaggio e quindi, parallelamente, accadono due cose: il valore che leggiamo viene nuovamente inviato su `valueStore` ed anche sul canale `ack`.

I messaggi sono tuple di nomi. Tutti i messaggi che abbiamo visto finora hanno avuto arità 1, ma qui sulla linea 8, stiamo aspettando un messaggio di arità 2. La prima parte del pattern lega la variabile `newValue` alla prima parte del messaggio, mentre la seconda parte del pattern lega la variabile `ack` a un canale. Invece di inviare `value` su `valueStore` come abbiamo fatto nella riga 7, inviamo `newValue`.

Sempre sulla linea 8, inviamo un messaggio di arità 0. Nell'esempio di utilizzo, le righe 20 e 22 usano un underscore per ricevere quel messaggio vuoto e scartarlo.

9) A questo punto, c'è di nuovo esattamente un messaggio sul canale `valueStore`.

13-31) Il codice di utilizzo dimostra la creazione di una cella, assegnando il valore iniziale 123, ottenendo e stampando quel valore, impostando il valore su 456, quindi ottenendo e stampando quel valore.  

Nota i livelli profondi di callback. Rholang è stato progettato per rendere i calcoli paralleli naturali da esprimere; di conseguenza, le dipendenze dei dati implicite nel sequenziamento in altre lingue devono essere rese esplicite.

## Iterazione 

    1 contract @"IterateDemo"(system) = new chan in {
    2     [1,2,3].iterate(chan) |
    3     for (@num, ack << chan) system!("print", num, *ack)
    4 }

2) Le parentesi grafe indicano una lista. Le liste sono mutabili, mentre le tuple, indicate con parentesi, non lo sono. Alcuni processi, come quelli supportati da oggetti Java, hanno metodi; qui, il metodo `iterate` viene invocato con un canale su cui eseguire l'iterazione.

3) L'operatore `<<` indica "invio sequenziale", o in altre parole, che i messaggi di chan necessitano di conferma prima che i nuovi messaggi vengano inviati.  

Il metodo `print` del processo di sistema può richiedere uno o due argomenti. Nel caso a due parametri, il secondo argomento è un canale sul quale verrà inviato un messaggio di conferma una volta completata la stampa. Il metodo `iterate` riceverà quel riconoscimento e invierà il messaggio successivo. Una volta che l'elenco ha terminato l'iterazione, la riga 3 si evolverà nel processo `Nil` che non fa nulla ed finiscec in garbage collection.

## Parametri di corrispondenza e di riposo del modello

     1 contract @"CoatCheckDemo"(system) = new MakeCoatCheck in {
     2     contract MakeCoatCheck(ret) = {
     3         new (portIn, portOut):iopair, table in {
     4             ret!(*portOut) |
     5             for (@method, ack, ...@rest <= portIn) {
     6                 match method {
     7                     case "new" => match rest {
     8                         case (initialValue) => new ticket in {
     9                             ack!(*ticket) |
    10                             @(*ticket | *table)!(initialValue)
    11                         }            
    12                     }
    13                     case "get" => match rest {
    14                         case (ticket) => {
    15                             for (@value <! @(*ticket | *table)) {
    16                                 ack!(value)
    17                             }
    18                         }
    19                     }
    20                     case "set" => match rest {
    21                         case (store, @newValue) => {
    22                             for (_ <- @(*ticket | *table)) {
    23                                 @(*ticket | *table)!(newValue) |
    24                                 ack!()
    25                             }
    26                         }
    27                     }
    28                 }
    29             }
    30         }
    31     } |
    32 
    33     // Usage
    34     new ret in {
    35         MakeCoatCheck(ret) |
    36         for (cc <- ret) {
    37             // Creates new cell with initial value 0
    38             cc!("new", *ret, 0) |
    39             for (ticket <- ret) {
    40                 // Sets the cell to 1
    41                 cc!("set", *ret, *ticket, 1) |
    42                 for (ack <- ret) {
    43                     // Reads the value
    44                     cc!("get", *ret, *ticket) |
    45                     for (@storedValue <- ret) {
    46                         // Prints 1
    47                         system!("print", storedValue)
    48                     }
    49                 }
    50             }
    51         }
    52     }
    53 }

2) Uno schema di progettazione, utilizzato nel contratto MakeCell mostrato sopra, è di ricevere dal chiamante un canale per ogni diversa funzionalità fornita da un processo. Un programmatore orientato agli oggetti potrebbe dire che MakeCell richiede al chiamante di fornire un canale per ogni metodo. Le partite vengono tentate nell'ordine in cui appaiono nel codice; se non si verifica alcuna corrispondenza, il blocco `match` valuta il processo `Nil`.

3) MakeCoatCheck utilizza un approccio più orientato agli oggetti. La costruzione `(in, out):iopair` ci consente di creare una coppia di canali accoppiati. Si tratta di un errore di tipo per inviare un messaggio tramite il canale `in` o per ricevere un messaggio tramite il canale `out`.  Tuttavia, qualsiasi messaggio inviato tramite il canale `out` può essere ricevuto tramite il canale `in`.  Questo ci consente di restituire il canale `out` su cui è possibile effettuare delle “chiamate al metodo" senza consentire ad altri processi di intercettare le richieste per il nostro processo.

Il nuovo canale `table` sarà usato per creare canali per uso interno.

5) Leggiamo ripetutamente nei messaggi qui di qualsiasi ordine maggiore o uguale a due. La variabile `method` viene associata al primo argomento; ci aspettiamo una stringa che nomini il metodo, proprio come fa il processo di sistema. La variabile `ack` viene associata a un canale su cui invieremo qualsiasi risultato della chiamata al metodo. La variabile `rest` viene associata a una tupla contenente il resto delle parti del messaggio.

6) La costruzione `match ... case` ci permette di creare una corrispondenza con il modello sulla struttura di un processo. Lo usiamo per inviare messaggi.

7-12) Se `method` è la stringa `"new"`, supponiamo alla riga 8 che `rest` sia una tupla con un elemento, il valore iniziale. Creiamo un canale `ticket` e lo restituiamo usando il canale `ack`.  Combiniamo anche i processi `*ticket` con `*table` in un nuovo processo e deriviamo da essi un nome di canale. Dal momento che solo noi abbiamo accesso a `table`, solo noi possiamo manipolare i dati memorizzati su canali costruiti in questo modo. Il `ticket` del canale si comporta come un biglietto riscattabile per un cappotto controllato, e il canale `@(*ticket | *table)` è la voce della tabella indicata da quel tasto.

13-19) Se `method` è la stringa `"get"`, allora assumiamo sulla riga 14 che `rest` sarà una tupla con un elemento, il particolare ticket per l'oggetto da recuperare. L'operatore `<!` Legge un valore dalla tabella e lo rimette immediatamente; questo è,

    for (y <! x) { P }

is syntactic sugar for

    for (y <- x) { x!(*y) | P }.

20-27) Se `method` è la stringa `"set"`, supponiamo alla riga 21 che `rest` sia una tupla con due elementi: la chiave e il nuovo valore. La riga 22 elimina il valore corrente su quel biglietto, la riga 23 invia il nuovo valore e la linea 24 segnala che è stata eseguita.

## Gestione degli errori

     1 for (@info, ret, err <- channel) {
     2     // Either return a result on ret or an error on err
     3 } |
     4 select {
     5     result <- ret => {
     6         // Process result
     7     }
     8     // Messages on err that don't fit this pattern
     9     // aren't intercepted here.
    10     @"TypeError", msg <- err => {
    11         // Handle type error
    12     }
    13 }

1-3) Possiamo specificare più canali su cui i dati possono essere inviati a un cliente.

4) In questo contesto, `select` si comporta come `try` in altre lingue. Solo uno dei ricevitori sulle linee 5 e 10 procederà; fanno a gara per vedere quale riceve prima un messaggio. Se le linee 1-3 hanno un invariante che viene inviato un risultato su `ret` o un errore su `err`, allora non ci sarà una gara. Se, al contrario, vogliamo inviare sia un risultato sia un errore, dovremmo usare `for` invece:

    for (@info, ret, err <- channel) {
        // Either return a result on ret or an error on err
    } |
    for(risultato <- ret) {
        // Process result
    } |
    for(@"TypeError", msg <- err) {
        // Handle type error
    }

10) Il modello qui è più complicato di quelli che abbiamo visto prima. Qui, specifichiamo che vogliamo solo messaggi con due nomi, e il primo nome dovrebbe essere la serializzazione della stringa `"TypeError"`.  Se è qualcos'altro, questo ramo non procederà.

## Filosofi a cena e stallo

     1 new north, south, knife, fork in {
     2     north!(knife) |
     3     south!(fork) |
     4     for (knf <- north) for (frk <- south) {
     5         philosopher1!(knf, frk)
     6     } |
     7     for (frk <- south) for (knf <- north) {
     8         philosopher2!(knf, frk)
     9     }
    10 }

Il problema dei filosofi a cena vede due filosofi condividere un solo set di posate. Philosopher1 siede sul lato est del tavolo, mentre Philosopher2 si trova a ovest. Ognuno ha bisogno sia di un coltello che di una forchetta per mangiare. Ognuno si rifiuta di rinunciare a un utensile finché non li ha usati entrambi per prendere un boccone. Se entrambi i filosofi raggiungono per primo l'utensile alla loro destra, entrambi moriranno di fame: Philosopher1 prende il coltello, Philosopher2 prende la forchetta e nessuno dei due lascerà mai andare la propria posata.

Ecco come risolvere il problema:

     1 new north, south, knife, spoon in {
     2     north!(knife) |
     3     south!(spoon) |
     4     for (knf <- north; spn <- south) {
     5         philosopher1!(knf, spn)
     6     } |
     7     for (spn <- south; knf <- north) {
     8         philosopher2!(knf, spn)
     9     }
    10 }

4, 7) L'operatore join, indicato con un punto e virgola `;`, dichiara che la continuazione dovrebbe procedere solo se c'è un messaggio disponibile su ciascuno dei canali simultaneamente, impedendo lo stallo descritto sopra.

## Tipi integrati, invio permanente, collegamenti logici e filtri

Abbiamo visto che i pattern che possiamo usare nella costruzione `match` o nella costruzione `for` includono processi con variabili libere. Possiamo anche usare schemi che descrivono processi incorporati. Il pattern `Integer` descrive tutti gli interi con segno a 32 bit; allo stesso modo per `Double`, `String`, e `Boolean`.

Possiamo combinare i modelli usando i connettivi logici AND, OR e NOT, indicati rispettivamente con `&&`, `||`, e `~`.

    for (@(x && Integer) <- y) { P }

Questo processo associa una variabile di processo `x` al messaggio ricevuto su `y`, ma insiste anche sul fatto che `x` è un numero intero.

C'è un indovinello americano che dice: "Come fai a cambiare quindici centesimi quando una moneta non è un nichelino e l'altra non è un decino?" Un nichelino vale cinque centesimi, e l’altra ne vale dieci. Supponiamo di avere i seguenti messaggi inviati su `coins`, che codificano i tipi disponibili di monete americane con valore inferiore a quindici centesimi:

    coins!!(1) | coin!!(5) | coin!!(10)

L'operatore `!!` significa che i messaggi dovrebbero rimanere permanentemente sul canale e non essere consumati quando vengono ricevuti da una costruzione `for`.

    new x in { x!!("Hi there!") | for (msg <= x) { system!("print", msg) } }

Il processo sopra stamperà "Hi there!" per tutto il tempo in cui la macchina virtuale rimarrà in esecuzione.

Possiamo codificare l'enigma come

    per (@(x && ~5) <- coins; @(y && ~10) <- coins if x+y == 15) {
        system!("print", (x, y))
    }

Il primo pattern, `@(x && ~5)`, corrisponderà a qualsiasi messaggio su `coins` che non è 5 e associa `x` ad esso; allo stesso modo, il secondo pattern `@(y && ~10)` corrisponderà a qualsiasi messaggio su `coins` che non è 10 e vincolerà `y` ad esso. La clausola `if` in una costruzione `for` consente solo alla partita di procedere se la formula a destra di essa valuta `true`; in questo caso, nulla verrà stampato a meno che i valori non raggiungano il valore di 15.  

La risposta all'enigma è "Un decino e un nichelino", poiché un decino non è un nichelino e un nichelino non è un decino. La variabile `x` si legherà a 10 e la variabile `y` si legherà a 5, e `(10, 5)` sarà stampato.

## Modelli di design sicuri

In questa sezione descriviamo diversi modelli di design. Questi schemi sono adattati di Marc Stiegler [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf).

### Facet

Nel contratto MakeCell, il client fornisce due canali, uno per ottenere il valore e uno per impostarlo. Se il client passa quindi solo il canale `get` a un altro processo, quel processo ha effettivamente una vista di sola lettura della cella.  

Canali come `get` e `set` sono chiamati "facce" del processo. Incapsulano l'autorità per eseguire l'azione. Se il canale `set` è un canale pubblico come `@"Foo"`, allora chiunque possa imparare o persino indovinare la stringa `"Foo"` ha l'autorità per impostare il valore della cella. D'altra parte, se il canale `set` è stato creato con l'operatore `new`, allora non c'è modo per nessun altro processo di costruire il canale `set`; deve essere passato a un processo direttamente affinché il processo lo usi.  

Nota che se `get` e `set` non vengono creati come metà di iopairs, allora il possesso di quei canali è anche l'autorità per intercettare i messaggi inviati alla cella:

    for (ret <- get) { P } | 
    for (ret <- get) { Q } | 
    get!(*ack)

Questo termine ha due processi in ascolto sul canale get e un singolo messaggio inviato su `get`.  Solo uno dei due processi sarà in grado di ricevere il messaggio.

Ricevendo i canali dal client per ricevere ed impostare, il contratto MakeCell lascia le decisioni su quanto questi canali siano pubblici verso il cliente. Il contratto MakeCellFactory, d'altra parte, costruisce i propri canali e li restituisce al cliente, quindi è in grado di far rispettare le garanzie sulla privacy.

### Attenuazione dei forwarders

Nel contratto MakeCellFactory, c'è un solo canale e i messaggi vengono inviati internamente. Per ottenere lo stesso effetto di un aspetto di sola lettura, possiamo creare un processo di inoltro che semplicemente ignora tutti i messaggi che non desidera inoltrare. Il contratto sotto inoltra solo il metodo "ottieni".

    contract MakeGetForwarder(target, ret) = {
        new (portIn, portOut):iopair in {
            ret!(*portOut) |
            for (@method, ...@rest <= portIn) {
                method match {
                    case "get" => target!(method, ...rest)
                }
            }
        }
    }

### Revoca

Possiamo implementare la revoca creando un forwarder con uno kill switch.

     1 contract MakeRevokableForwarder(target, ret) = {
     2     new (portIn, portOut):iopair, kill, forwardFlag in {
     3         ret!(*portOut, *kill) |
     4         forwardFlag!(true) |
     5         for (...@rest <= portIn) {
     6             for (@status <! forwardFlag) {
     7                 if (status) {
     8                     target!(rest)
     9                 } else {
    10                     Nil
    11                 }
    12             }
    13         } |
    14         for (_ <- kill; _ <- forwardFlag) {
    15             forwardFlag!(false)
    16         }
    17     }
    18 }

2) Creiamo un iopair per l'invio del metodo e un canale `forwardFlag` per memorizzare se inoltrare i messaggi.

3) Restituiamo il canale su cui i client inviano richieste e il canale su cui inviare il segnale kill.

4) Impostiamo lo stato iniziale di `forwardFlag` su true.

5-13) Leggiamo in una tupla arbitraria di parti di messaggi e otteniamo il valore della bandiera. Se il flag è true, inoltriamo la tupla del messaggio a `target`.

14-15) Se un messaggio viene mai inviato sul canale `kill`, impostiamo `forwardFlag` su false, che interrompe l'inoltro dei messaggi.

### Composizione

Combinando uno forwarder attenuante con un forwarder revocabile, otteniamo entrambe le funzionalità:

    new ret in {
        MakeGetForwarder(target, *ret) |
        for (@getOnly, kill <- ret) {
            MakeRevokableForwarder(getOnly, *ret) |
            for (@revokableGetOnly <- ret) {
                // give away revokableGetOnly instead of target
                // hang onto kill for later revocation
            }
        }
    }

### Logging forwarder

Un Logging forwarder può registrare tutti i messaggi inviati su un canale replicandoli su un secondo canale.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new (portIn, portOut):iopair in {
            ret!(*portOut) |
            for (...@rest <= portIn) {
                target!(...rest) |
                logger!(...rest)
            }
        }
    }

### Responsabilità

Immagina che Alice abbia un canale e desideri registrare l'accesso di Bob ad esso. Bob vorrebbe delegare l'uso di quel canale a Carol e registrare il suo accesso. Ciascuna delle parti è libera di costruire il proprio server di inoltro del registro attorno al canale che ha ricevuto. Alice considererà Bob responsabile per qualsiasi cosa Carol faccia.

### Sigillare e togliere il sigillo

     1 contract MakeSealerUnsealer(ret) =  {
     2     new (sealerIn, sealerOut):iopair,
     3         (unsealerIn, unsealerOut):iopair,
     4         mapRet in {
     5 
     6         ret!(*sealerOut, *unsealerOut) |
     7         MakeCoatCheck(mapRet) |
     8         for (cc <- mapRet) {
     9             for (@value, ret <= sealerIn) {
    10                 cc!("new", *ret, value)
    11             } |
    12             for (ticket, ret <= unsealerIn) {
    13                 cc!("get", *ret, *ticket)
    14             }
    15         }
    16     }
    17 }

Una coppia sealer/unsealer fornisce la stessa funzionalità delle chiavi pubbliche, ma senza crittografia. È semplicemente un'attenuazione del controllo del cappotto descritto sopra. Questo modello di progettazione può essere utilizzato per firmare qualcosa per conto di un utente. Nel tutorial sulla blockchain di Rholang, vedremo che funziona anche sulla blockchain perché non ci sono segreti da archiviare, solo i nomi che non possono essere conservati sono inaccessibili.

### Attenzione all'invio di attenuatori

Un principio di base da tenere a mente con i processi RChain è che è simile a quello delle applicazioni web più tradizionali: qualsiasi codice inviato a un'altra parte può essere disassemblato. Fin dalla fine degli anni '90, quando è diventato possibile acquistare cose sul web, [ci sono state piattaforme di e-commerce](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/) in cui la piattaforma si è basata sui browser degli utenti per inviare di nuovo il prezzo corretto dell'articolo. Gli autori non pensavano che l'utente avrebbe aperto gli strumenti di sviluppo e modificato il prezzo prima di essere rispedito. Il modo giusto per costruire una piattaforma di e-commerce è quello di memorizzare i prezzi sul server e controllarli lì.

Supponiamo che Bob sia disposto a eseguire alcuni codici per Alice; ha un contratto che dice qualcosa come "Prendi un processo da questo canale ed eseguilo".

    for (@P <- x) { P }

Questo è come se un browser web fosse disposto a eseguire il codice JavaScript che ottiene da un sito web. Se Alice invia a Bob un forwarder attenuante, Bob può utilizzare le produzioni di pattern matching in Rholang per smantellare il processo e ottenere l'accesso alla risorsa sottostante. Invece, come nell'esempio di e-commerce, Alice dovrebbe inviare solo il codice che inoltra richieste ai suoi processi e ne esegue l'attenuazione.

## Conclusione

RChain è un linguaggio progettato per l'uso su una blockchain, ma non abbiamo menzionato nulla su nodi, namespace, portafogli, Rev e phlogiston, struttura di rete o Casper. Un prossimo documento affronterà tutti questi problemi e altro ancora.

Speriamo che i precedenti esempi suscitino il desiderio di scrivere più codice e dimostrare la facilità di esprimere progetti concomitanti.
