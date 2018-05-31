# Ein Rholang-Tutorial 

Rholang ist eine neue Programmiersprache, die für den Einsatz in verteilten Systemen entwickelt wurde. Wie alle neugeborenen Dinge wächst und verändert es sich schnell; Dieses Dokument beschreibt die Syntax, die in der RNode-0.3-Version verwendet wird. 

Rholang ist "prozessorientiert": Alle Berechnungen werden mittels Message Passing durchgeführt. Nachrichten werden über "Kanäle" weitergeleitet, die eher wie Nachrichtenwarteschlangen sind, sich aber wie Sets statt Warteschlangen verhalten. Rholang ist völlig asynchron, in dem Sinne, dass Sie zwar eine Nachricht von einem Kanal lesen und dann etwas damit tun können, aber keine Nachricht senden und dann etwas tun können, sobald sie empfangen wurde - zumindest nicht ohne explizit Warten auf eine Bestätigungsnachricht vom Empfänger. Beachten Sie, dass in diesem Dokument die Wörter "Name" und "Kanal" austauschbar verwendet werden. Dies liegt daran, dass im Rho-Calculus (auf dem Rholang basiert) der Begrif Name verwendet wird, da Sie jedoch Informationen über Namen senden und empfangen können, semantisch sind sie wie Kanäle.

## Anfangen 

Es gibt keine IDE für Rholang. Beginnen Sie mit Rholang, indem Sie eine der folgenden Optionen auswählen. 
* __Führen Sie Rholand auf RNode aus__ - Schreiben Sie Rholang-Verträge in einem Editor Ihrer Wahl und führen Sie sie auf RNode mit den REPL- oder EVAL-Modi aus. [Beginnen Sie](https://github.com/rchain/rchain/releases) mit der neuesten Version von RNode.
* __Ausführen von Rholang auf einer Weboberfläche__ - Diese [Webschnittstelle](http://rchain.cloud) wurde von einem RChain-Community-Mitglied erstellt.
* __Schreiben Sie Rholang mit einem IntelliJ-Plugin__ - Dieses [Rholang IntelliJ plugin](https://github.com/tgrospic/rholang-idea) wurde von einem RChain-Community-Mitglied erstellt.

## Verträge und Senden von Daten

    1 new HelloWorld in {
    2   contract HelloWorld(return) = {
    3     return!("Hello, World!")
    4   } |
    5   new myChannel in {
    6     HelloWorld!(*myChannel)
    7   }
    8 }

1-2) Um einen neuen, privaten Kanal zu erstellen, verwenden wir die `new ... in` Konstruktion. Kein anderer Prozess kann Nachrichten über diesen Kanal senden oder empfangen, es sei denn, wir senden diesen Kanal explizit an den anderen Prozess. Dieser Prozess beginnt, indem Sie einen neuen Namen `HelloWorld`erstellen und dann einen Vertrag für ihn ausführen. Die `contract` Produktion erzeugt einen Prozess, der eine Kopie seines Körpers hervorbringt, wenn er eine Nachricht erhält.

3) Auf dem Rückkanal senden wir einen Prozess, der die Zeichenfolge "Hello, World!"Ist. 

6) Wir senden den Kanal `myChannel` zum Vertrag bei `HelloWorld`. Der `*` Operator "hebt" einen Kanal auf, um den zugrunde liegenden Prozess zu erhalten. In Roholang können Sie Prozesse nur über Kanäle senden.Sie können Kanäle nicht direkt über Kanäle senden.Daher verwenden wir `*` , um den privaten Kanal vor dem Senden in einen Prozess zu verwandeln.


## Daten empfangen

    1 new HelloAgain in {
    2   contract HelloAgain(_) = {
    3     new chan in {
    4       chan!("Hello again, world!") |
    5       for (@text <- chan) { Nil }
    6     }
    7   } | HelloAgain!(Nil)
    8 }


2) Verträge nehmen mindestens einen Parameter an, aber wir können ihn wegwerfen, indem wir ihn an den Platzhalter `_`binden.

3) Wir erstellen einen neuen Kanal `chan`.

4) Wir senden den String-Prozess `"Hello again, world!"` über den neuen Kanal.

5) Wir hören auf den neuen Kanal für eine einzelne Nachricht. Die Operation`for` blockiert solange, bis eine Nachricht auf dem Kanal `chan`verfügbar ist.In Rholang können Sie nur Namen auf Kanälen empfangen (beachten Sie, dass dies vom Senden abweicht!). Die Bindung auf der linken Seite von `<-` in der `for` is actually a name pattern. In diesem Beispiel ist das Muster `@text`, was bedeutet, dass der empfangene Name ein in Anführungszeichen gesetzter Prozess ist und wir diesen Prozess an den freien variablen `text`binden wollen.Die Operation`for` ist wie ein Vertrag, außer dass sie nur eine Nachricht liest und dann zu ihrem Körper wird, anstatt eine Kopie ihres Körpers für jede Nachricht zu fälschen.In diesem Fall wählen wir, nichts im `for` Körper zu tun, indem wir einfach den angehaltenen Prozess`Nil`machen, aber im Prinzip würden wir mit einer weiteren Verarbeitung des in`chan` enthaltenen `text`fortfahren wollen. 

7) Wir lösen den Vertrag aus.

## Veränderlicher Zustand

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

1) Wir erstellen einen neuen Kanal MakeCell und verwenden ihn dann in Zeile 3 als Namen eines internen Vertrags. Kein anderer Prozess als der Code innerhalb dieses lexikalischen Bereichs kann ihn aufrufen. 

3) Der `MakeCell` Vertrag benötigt drei Argumente. Das erste Argument ist der Anfangswert, der in der Zelle gespeichert werden soll. Das zweite und dritte Argument sind Kanäle, über die die Zelle Anforderungen zum Abrufen und Festlegen des Werts erhält. Beachten Sie, dass das erste Argument ein Prozess und das zweite und dritte ein Name sein sollen. Namen werden jedoch immer über Kanäle empfangen. Daher müssen wir das erste Argument mit `@` beginnen, um anzuzeigen, dass der Name, den wir als erstes Argument erhalten, ein zitierter Prozess ist und dass dieser Prozess an die Variable gebunden werden soll.

4) Um den Wert zu speichern, erstellen wir einen neuen Kanal. Dieser Kanal enthält höchstens eine Nachricht, die den aktuellen Wert der Zelle enthält.   

5) Vor dieser Zeile befinden sich keine Meldungen im `value store` Kanal. Nachdem wir den Anfangswert gesendet haben, ist dies der einzige Wert auf diesem Kanal.

6) Wir haben einen Vertrag geschlossen, um den `get Kanal` anzuhören. Jedes Mal, wenn eine Nachricht an `get` gesendet wird, wird der Vertragskörper ausgeführt 

7) Wir blockieren, bis wir eine Nachricht vom `value store` Kanal erhalten. Da höchstens eine Nachricht auf dem `value store` wartet, verhält sich das Lesen der Nachricht ähnlich wie das Erfassen einer Sperre. 

8)  Wir senden den aktuellen Wert erneut an `valueStore`, wodurch andere Nachrichten verarbeitet werden können (d. H. Die Sperre aufgehoben wird), und wir senden den aktuellen Wert zurück an den Client auf dem `ack` Kanal. 

11) Gleichzeitig mit dem `get` Vertrag führen wir einen Vertrag, der am `set` überwacht wird. 

12) Wir blockieren, bis eine Nachricht im `valueStore` vorhanden ist, und lesen sie dann. Wir werfen die Nachricht weg, die wir lesen. 

13) Wir senden den neuen Wert zum Speichern im `valueStore` und signalisieren, dass der Vorgang abgeschlossen ist. 

18-36) Der Verwendungscode demonstriert das Erstellen einer Zelle, die Zuweisung des Anfangswerts 123, das Abrufen dieses Werts, das Festlegen des Werts auf 456 und das Abrufen dieses Werts.

Beachten Sie die tiefen Schichten des Rückrufs. Rholang wurde entwickelt, um gleichzeitige Berechnungen zum Ausdruck zu bringen. Folglich müssen Datenabhängigkeiten, die bei der Sequenzierung in anderen Sprachen enthalten sind, explizit gemacht werden.

## Iteration und Abgleich

Im folgenden Code zeigen wir ein Beispiel für das Durchlaufen einer Liste.

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
    16	      /* handle processing of item */
    17	      ack!(Nil)
    18	    } |
    19	    for (_ <- done) {
    20	      /* done! */
    21	      Nil
    22	    }
    23	  }
    24	}

3) Die `match` Konstruktion ermöglicht das Destrukturieren einer Variablen durch Mustervergleich.

4) Listenmuster unterstützen den Rest einer Liste. Wenn `list` mit dem Muster eines Head / Tail-Paars übereinstimmt, führen wir den Hauptteil der Schleife aus. 

5) Wir erstellen einen Kanal für den Artikel-Handler, um uns mitzuteilen, dass es mit dem aktuellen Artikel gemacht wurde. 

6) Wir rufen den Prozessor auf dem Element und dem Bestätigungskanal auf. 

7) Wenn wir eine Bestätigung erhalten, rufen wir den Iterator erneut auf. 

10) Wenn die Liste leer ist, signalisieren wir, dass die Verarbeitung abgeschlossen ist.

14) Wir rufen den Iterator auf. 

15-18) Dieser `contract` wird für jede Position in der Liste aufgerufen. In Zeile 17 teilen wir dem Iterator mit, dass wir mit diesem Element fertig sind. 

19) Dies `for` enthält den Code, der ausgeführt werden soll, wenn die Interaktion abgeschlossen ist.

## Karten

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

2) Ein Entwurfsmuster, das im obigen MakeCell-Vertrag verwendet wird, besteht darin, vom Aufrufer einen Kanal für jede andere Funktionalität zu erhalten, die ein Prozess bereitstellt. Ein objektorientierter Programmierer könnte sagen, dass MakeCell erfordert, dass der Aufrufer für jede Methode einen Kanal bereitstellt. MakeCoatCheck verwendet einen objektorientierten Ansatz, wie wir sehen werden.

3) Wir erstellen einen `port` Kanal für die Interaktion mit dem Coat-Check sowie eine `table` Name, die beim Speichern / Abrufen von Werten in den Coat-Check verwendet wird.

4) Wir senden `port` an den Anrufer, damit sie mit dem Coat-Check interagieren können.

5, 11, 17) Wir definieren verschiedene Methoden, die durch Senden einer Nachricht an `port` aufgerufen werden können. Dies geschieht durch die Angabe sich gegenseitig ausschließender Muster, die die Nachricht am `port` erfüllen kann, wobei das erste Element der Nachricht der Methodenname ist und nachfolgende Elemente das Argument (die Argumente) und der Rückkanal sind. Wenn Sie den `<=` Pfeil anstelle des `<-` Pfeils verwenden, werden die `for`s "repliziert". Dies gibt ihnen das gleiche Verhalten wie `contract`, d. H. Der Prozess, der auf Nachrichten auf dem `port` wartet, bleibt bestehen, nachdem eine Instanz seines Körpers erzeugt wurde.

8) Wir nutzen die Möglichkeit, einen beliebigen Prozess zu zitieren, um einen Namen zu erstellen, um einen eindeutigen Namen für jeden zu speichernden Wert zu erstellen. Der Prozess `*ticket | *table` wird durch die gleichzeitige Zusammensetzung der Prozesse erzeugt, die durch das Aufheben der Nennung des Namens `ticket` und `table` erzeugt werden. Dieser Prozess kann dann zitiert werden, um einen eindeutigen Namen zu bilden, der dann verwendet wird, um den Wert zu speichern, indem er an den Namen gesendet wird.

## Die Essenden Philosophen und Stillstand

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

Das Philosophen-Problem hat zwei Philosophen, die nur einen Satz Silberbesteck teilen. Philosopher1 sitzt auf der Ostseite des Tisches, während Philosopher2 auf der Westseite sitzt. Jeder braucht sowohl ein Messer als auch einen Löffel, um zu essen. Jeder weigert sich, ein Utensil abzugeben, bis er beides benutzt hat, um einen Bissen zu nehmen. Wenn beide Philosophen zuerst nach dem Utensil zu ihrer Rechten greifen, verhungern beide: Philosoph1 bekommt das Messer, Philosopher2 bekommt den Löffel, und beide lassen nie los.

So lösen Sie das Problem:

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

4, 9) Der mit einem Semikolon `;`gekennzeichnete Join-Operator erklärt,dass die Fortsetzung nur fortgesetzt werden soll, wenn gleichzeitig auf jedem der Kanäle eine Nachricht verfügbar ist, die den obigen Stillstand verhindert.

## Sichere Designmuster

In diesem Abschnitt beschreiben wir mehrere Entwurfsmuster. Diese Patterns stammen aus Marc Stieglers [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf).

### Facetten

Im MakeCell-Vertrag stellt der Client zwei Kanäle bereit, einen zum Abrufen des Werts und einen zum Festlegen. Wenn der Client dann nur den `get` Kanal an einen anderen Prozess übergibt, verfügt dieser Prozess über eine schreibgeschützte Ansicht der Zelle. 

Kanäle wie `get` und `set` heißen "Facetten" des Prozesses. Sie kapseln die Berechtigung zum Ausführen der Aktion ein. Wenn der `set` Kanal ein öffentlicher Kanal wie `@"Foo"`,  ist, dann kann jeder, der die Zeichenfolge `"Foo"` lernen oder sogar erraten kann, den Wert der Zelle festlegen. Auf der anderen Seite, wenn der gesetzte Kanal mit dem neuen Operator erstellt wurde, gibt es keine Möglichkeit für irgendeinen anderen Prozess, den `set` Kanal zu konstruieren; es muss direkt an einen Prozess übergeben werden, damit der Prozess es verwenden kann.  

Beachten Sie, dass der Besitz von `get` und `set` auch die Berechtigung zum Abfangen von Nachrichten hat, die an die Zelle gesendet werden:

    for (@ret <- get) { P } | 
    for (@ret <- get) { Q } | 
    get!(*ack)

Dieser Begriff hat zwei Prozesse, die auf den Kanal `get` hören und eine einzelne Nachricht über `get` senden. Nur einer der beiden Prozesse kann die Nachricht empfangen. 

Durch den Empfang von Kanälen vom Client zum Abrufen und Festlegen, lässt der MakeCell-Vertrag die Entscheidungen darüber, wie öffentlich diese Kanäle für den Client sind, zu. Der MakeCoatCheck-Vertrag dagegen baut seine eigenen Kanäle auf und legt Methoden dem Kunden offen, so dass er in der Lage ist, Datenschutzgarantien durchzusetzen.

### Dämpfung von Forwarders

Im MakeCoatCheck-Vertrag gibt es nur einen Kanal und Nachrichten werden intern versandt. Um denselben Effekt wie eine schreibgeschützte Facette zu erzielen, können wir einen Forwarder Prozess erstellen, der einfach alle Nachrichten ignoriert, die nicht weitergeleitet werden sollen. Der nachfolgende Vertrag gibt nur die "Get" -Methode weiter.

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
    
### Widerruf

Wir können die Sperrung implementieren, indem wir ein Forwarder mit einem Kill-Schalter erstellen.

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

3) Wir erstellen einen Port zum Abhören von Methodenaufrufen und einen Kanal `forwardFlag`, um zu speichern, ob Nachrichten weitergeleitet werden sollen.

4) Wir geben den Kanal zurück, auf dem Clients Anfragen senden, und den Kanal, auf dem das Kill-Signal gesendet werden soll.

5) Wir setzen den Anfangszustand von `forwardFlag` auf true.

6-11) Wir lesen eine beliebige Nachricht ein, holen und ersetzen den Wert des Flags. Wenn das Flag wahr ist, leiten wir die Nachricht an `target`weiter.

12-14) Wenn eine Nachricht jemals auf dem `kill`-Kanal gesendet wird, setzen wir `forwardFlag` auf false. Der Forwarder-Prozess stoppt dann die Weiterleitung von Nachrichten.

### Zusammensetzung

Durch die Kombination eines dämpfenden Forwarders mit einem widerruflichen Forwarder erhalten wir beide Funktionen:

    new ret in {
      MakeGetForwarder(target, ret) |
      for (@pair <- ret) {
        match pair {
          [getOnly, kill] => {
            MakeRevokableForwarder!(getOnly, *ret) |
            for (revokableGetOnly <- ret) {
              // give away revokableGetOnly instead of target
              // hang onto kill for later revocation
            }
          }
        }
      }
    }

### Logging forwarder

Ein Logging Forwarder kann alle auf einem Kanal gesendeten Nachrichten aufzeichnen, indem sie auf einem zweiten Kanal wiedergegeben werden.

    contract MakeLoggingForwarder(target, logger, ret) = {
      new port in {
        ret!(*port) |
        contract port(@msg) = {
          target!(msg) |
          logger!(msg)
        }
      }
    }

### Rechenschaftspflicht

Angenommen, Alice hat einen Kanal und möchte Bobs Zugriff darauf protokollieren. Bob möchte die Nutzung dieses Kanals an Carol delegieren und ihren Zugriff protokollieren. Jede Partei kann ihren eigenen Logging-Forwarder um den empfangenen Kanal herum aufbauen. Alice wird Bob für das, was Carol tut, verantwortlich machen.

### Sealing und Unsealing

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


Ein Sealer / Unsealer-Paar bietet die gleiche Funktionalität wie öffentliche Schlüssel, jedoch ohne Kryptographie. Es ist lediglich eine Abschwächung der oben beschriebenen Coat-Check. Dieses Entwurfsmuster kann benutzt werden, um etwas für den Benutzer zu unterzeichnen. Im Rohling-Blockchain-Tutorial werden wir sehen, dass ein Sealer / Unsealer -Paar sogar als Signier / Verifizierungs-Schlüsselpaar in der Blockchain funktioniert, da es keine Geheimnisse zum Speichern gibt, sondern nur fälschungssichere Namen, auf die nicht zugegriffen werden kann.

### Vorsicht beim Senden von Dämpfungsgliedern

Ein Grundprinzip, das bei RChain-Prozessen zu beachten ist, ist ähnlich wie bei herkömmlichen Webanwendungen: Der Code, den Sie an eine andere Partei senden, kann zerlegt werden. Seit den späten 1990er Jahren, als der Kauf von Dingen über das Internet möglich wurde, [gab es E-Commerce-Plattformen](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/)bei denen die Plattform auf die Browser der Benutzer angewiesen war, um den korrekten Preis des Artikels an ihn zurückzusenden. Die Autoren haben nicht daran gedacht, dass der Benutzer die Entwicklerwerkzeuge öffnet und den Preis ändert, bevor er zurückgeschickt wird. Der richtige Weg, um eine E-Commerce-Plattform aufzubauen, besteht darin, die Preise auf dem Server zu speichern und dort zu überprüfen.

Angenommen, Bob ist bereit, ein Code für Alice auszuführen; Er hat einen Vertrag, der so etwas sagt wie: "Bekommen Sie einen Prozess von diesem Kanal und führen Sie ihn aus."

    for (p <- x) { *p }

Dies ist wie ein Webbrowser, der bereit ist, den JavaScript-Code auszuführen, den er von einer Website erhält. Wenn Alice Bob einen dämpfenden Forwarder sendet, kann Bob die Pattern-Matching-Produktionen in Rholang verwenden, um den Prozess zu zerlegen und Zugriff auf die zugrunde liegende Ressource zu erhalten. Stattdessen sollte Alice, wie im E-Commerce-Beispiel, nur Code senden, der Anfragen an ihre eigenen Prozesse weiterleitet und dort die Dämpfung durchführt.

## Schlussfolgerung

RChain ist eine Sprache, die für die Verwendung in einer Blockchain entwickelt wurde, aber wir haben nichts über Nodes, Namespaces, Wallets, Rev und Phlogiston, Netzwerkstruktur oder Casper erwähnt. Ein in Kürze erscheinendes Dokument wird sich mit all diesen Fragen und mehr befassen.

Wir hoffen, dass die vorstehenden Beispiele den Wunsch wecken, mehr Code zu schreiben und die Einfachheit des Ausdrucks gleichzeitiger Designs zu demonstrieren.


