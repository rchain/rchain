# Instrukcja Rholanga

Rholang to nowy język programowania przeznaczony do stosowania w systemach rozproszonych. Jak wszystkie nowonarodzone rzeczy, szybko się zmienia i rośnie; ten dokument opisuje składnię, która będzie używana w wersji 0.1 SDK.

Rholang jest "zorientowany na proces": wszystkie obliczenia odbywają się za pomocą przekazywania komunikatów. Wiadomości są przekazywane na "kanałach", które są kolejkami wiadomości, ale zachowują się jak zestawy, a nie kolejki. Rholang jest całkowicie asynchroniczny, w tym sensie, że podczas gdy możesz czytać wiadomość z kanału, a następnie coś z nią zrobić, nie możesz wysłać wiadomości, a następnie zrobić coś po otrzymaniu --- przynajmniej nie bez wyraźnego czekania na wiadomość potwierdzającą z odbiornika.

## Kontrakty i wysyłanie danych

    1 new helloWorld in {
    2   contract helloWorld(name) = {
    3     "Hello, ".display(name, "!\n")
    4   } |
    5   helloWorld("Joe")
    6 }

Program Rholang jest pojedynczym procesem. Ten proces rozpoczyna się od utworzenia nowego kanału o nazwie `helloWorld`. Aby utworzyć nowy, prywatny kanał, używamy konstrukcji `new ... in`. Żaden inny proces nie może wysyłać ani odbierać wiadomości za pośrednictwem tego kanału, chyba że wyraźnie wyślemy ten kanał do innego procesu.

2) Produkcja `contract` tworzy proces, który stwarza kopię swojej treśći za każdym razem, gdy otrzymuje wiadomość.

3) Metoda `display` łańcucha znaków wypisuje standardowe wyjście. Potrzeba listę łańcuchuw znaków do wydrukowania w następnej kolejności. Dlatego, aby to działało, komunikat `name` powinien być łańcuchem.

5) Wysyłamy  łańcuch znaków `"Joe"` nad kanałem `helloWorld`.

## Odbieranie danych

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

2) Kontrakty przyjmują co najmniej jeden parametr, ale możemy go wyrzucić, wiążąc go z zmienną, której nigdy nie używamy.

3) Tworzymy nowy kanał `chan`.

4) Wysyłamy proces ciąga znaków `"Hello again, world!"` na nowym kanale.

5) Słuchamy nowego kanału dla pojedynczej wiadomości. Operacja `for`, blokuje sie dopóki nie pojawi się komunikat na kanale` chan`. Operacja `for` jest podobna do umowy, z tą różnicą, że odczytuje tylko jedną wiadomość, a następnie staje się jej treścią zamiast rozgałęziać kopię jej treści dla każdej wiadomości.

## Stan zmienny

     1 new MakeCell in {
     2   // Tworzy pojedynczą komórkę, w której możesz przechowywać wartości
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
    20   // Używanie komórki.
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

1) Tworzymy nowy kanał MakeCell, a następnie używamy go na linii 3 jako nazwy umowy wewnętrznej. Żaden proces inny niż kod w tym zakresie leksykalnym nie może go wywołać.

3) Umowa `MakeCell` przyjmuje trzy argumenty. Pierwszy argument to wartość początkowa, która ma być przechowywana w komórce. Drugi i trzeci argument to kanały, przez które komórka odbierze żądania pobrania i ustawienia wartości.

4) Aby zapisać wartość, tworzymy nowy kanał. Ten kanał będzie miał najwyżej jedną wiadomość zawierającą obecną wartość komórki.

5) Przed tą linią nie ma żadnych wiadomości na kanale `valueStore`. Po wysłaniu wartości początkowej jest to jedyna wartość na tym kanale.

6) Zawarliśmy umowę, aby nasłuchiwać na kanale `get`. Za każdym razem, gdy wiadomość zostanie wysłana na `get`, treść kontraktu zostanie wykonana.

7) Blokujemy, dopóki nie otrzymamy jednej wiadomości z kanału "valueStore". Ponieważ w "ValueStore" jest co najwyżej jedna wiadomość oczekująca, czytanie komunikatu zachowuje się podobnie jak przy uzyskiwaniu zamka.

8) Ponownie wysyłamy bieżącą wartość od `valueStore`, pozwalając na przetwarzanie innych wiadomości i  wysyłamy bieżącą wartość do klienta na kanale` ack`.

11) Równolegle z umową `get` uruchamiamy umowe która odsłuchuje na `set`.

12) Blokujemy, dopóki nie pojawi się wiadomośc na `valueStore`, a następnie ją odczytamy. Odrzucamy wiadomość, którą odczytalizmy.

13) Operacja `match` przywraca wiązanie destruktury, dzieląc krótkę ` pair` na jej komponenty i przypisując im nazwy.

14) Wysyłamy nową wartość do przechowalni w `valueStore` i sygnalizujemy, że operacja została zakończona.

21-36) Kod użycia demonstruje utworzenie komórki, przypisanie wartości początkowej 123, pobranie i wydrukowanie tej wartości, ustawienie wartości na 456, a następnie pobranie i wydrukowanie tej wartości.

Zwróć uwagę na głębokie warstwy wywołania zwrotnego. Rholang został zaprojektowany, aby obliczenia równoległe były naturalne do wyrażania; w związku z tym zależnośći danych które są ukryte w sekwencjach w innych językach, muszą być wyraźne.

## Iteracja i dopasowanie

W poniższym kodzie, `iterate` najpierw wysyła kanał ` next` nad `iterator`, a następnie dla każdej wiadomości odebranej na ` next` wysyła parę zawierającą następny element na liście i czy iteracja jest wykonywana.

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
    20     // Wywołaj umowę iteracyjną na kanale
    21     iterate([4,5,6], iterator) |
    22     
    23     // Współdziała z iteratorem
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

7) Konstrukcja `match .. with` umożliwia wiązanie destrukturyzacji.

8) Metoda `nth` na krótkach pozwala na ekstrakcję poszczególnych elementów.

16) Krótki mają metodę `size`.

## Mapy

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
    34     // Użycie
    35     new ret in {
    36         MakeCoatCheck(ret) |
    37         for (cc <- ret) {
    38             // Tworzy nową komórkę z początkową wartością 0
    39             cc("new", ret, 0, Nil) |
    40             for (ticket <- ret) {
    41                 // Ustawia komórkę na 1
    42                 cc("set", ret, ticket, 1) |
    43                 for (ack <- ret) {
    44                     // Czyta wartość
    45                     cc("get", ret, ticket, Nil) |
    46                     for (storedValue <- ret) {
    47                         // Drukuje 1
    48                         storedValue.display("\n")
    49                     }
    50                 }
    51             }
    52         }
    53     }
    54 }

2) Jeden wzorzec projektowy, stosowany w powyższej umowie  MakeCell, polega na odbieraniu od dzwoniącego kanału dla każdego innego elementu funkcjonalności, który zapewnia proces. Programista obiektowy może powiedzieć, że MakeCell wymaga od wywołującego podania kanału dla każdej metody. Dopasowania są podejmowane w kolejności, w jakiej pojawiają się w kodzie; jeśli nie występuje dopasowanie, blok `match` ocenia do processa `Nil`.  MakeCoatCheck wykorzystuje podejście bardziej obiektowo oriętowane, jak zobaczymy.

3-4) Każde sprawdzanie płaszcza ma własną zmienną, wielobieżną mapę, w której można przechowywać przedmioty. Przechowujemy nowo utworzoną mapę w MapStore. Ma następujące API:

    insert(key, value)
    insertMany(key1, val1, key2, val2, ..., keyn, valn)
    getOrElse(key, default)
    get(key)

6) Za każdym razem oczekujemy czterech argumentów; moglibyśmy również oczekiwać pojedynczą krótke, i użyć wiązanie destruktury do wysyłki w oparciu zarówno o metodę, jak i długość krótki.

## Ucztująci filozofowie i deadlock

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

Problem ucztujących filozofów ma dwóch filozofów, którzy dzielą tylko jeden komplet sztućców. Filozof 1 siedzi po wschodniej stronie stołu, podczas gdy Filozof 2 siedzi na zachodniej stronie. Każdy potrzebuje zarówno noża, jak i widelca do jedzenia. Każdy odmawia opuszczenia naczynia, dopóki nie użyje obu do ugryzienia. Jeśli obaj filozofowie sięgną najpierw po naczynie po ich prawej stronie, obaj umrą z głodu: Filozof 1 dostaje nóż, Filozof 2 dostaje widelec i nikt ich nie puszcza.

Oto, jak rozwiązać problem:

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

4, 9) Operator łączenia, oznaczony średnikiem `;`, deklaruje, że kontynuacja powinna kontynuować tylko wtedy, gdy na każdym z kanałów jest dostępną wiadomośc równocześnie,  zapobiegając deadlocka powyżej.

## Bezpieczne wzorce projektowe

W tej sekcji opisujemy kilka wzorców projektowych. Wzory te są zaadaptowane z [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf) Marca Stieglera.

### Aspekty

W umowie MakeCell klient udostępnia dwa kanały, jeden do uzyskania wartości i drugi do ustawienia jej. Jeśli klient przekazuje tylko kanał `get` do innego procesu, proces ten efektywnie ma widok tylko-do-odczytu  tej komórki.

Kanały takie jak `get` i `set` są nazywane "aspektami" procesu.  Zawierają one autorytet do wykonania działania. Jeśli kanał `set` jest kanałem publicznym takim jak `@"Foo"`, wówczas każdy, kto może nauczyć się lub nawet odgadnąć łańcuch znaków `"Foo"` ma uprawnienia do ustawienia wartości komórki. Z drugiej strony, jeśli kanał `set` został utworzony za pomocą operatora `new` , wówczas żaden inny proces nie może zbudować kanału `set`; musi zostać przekazany do procesu bezpośrednio, aby proces mógł go użyć.

Zauważ, że posiadanie `get` i `set` jest także uprawnieniem do przechwytywania wiadomości wysyłanych do komórki:

    for (ret <- get) { P } | 
    for (ret <- get) { Q } | 
    get(ack)

Ten termin ma dwa procesy nasłuchujące na kanale `get` i pojedynczą wiadomość wysłaną przez `get`. Tylko jeden z dwóch procesów będzie mógł odebrać wiadomość.

Odbierając kanały od klienta w celu uzyskania i ustawienia, umowa MakeCell pozostawia decyzje o tym, jak publiczne są te kanały dla klienta. Z drugiej strony, umowa MakeCellFactory konstruuje własne kanały i zwraca je klientowi, dzięki czemu jest w stanie egzekwować gwarancje prywatności.

### Atenuacja spedytorów

W umowie MakeCellFactory jest tylko jeden kanał, a komunikaty są wysyłane wewnętrznie. Aby uzyskać taki sam efekt jak aspekt tylko-do-odczytu, możemy utworzyć proces spedytora który po prostu ignoruje wszystkie wiadomości, których nie chce przekazywać dalej. Poniższy kontrakt przekazuje jedynie metodę "get".

    contract MakeGetForwarder(target, ret) = {
        new port in {
            ret(port) |
            contract port(tuple) = {
                tuple.nth(0) match with "get" => target(tuple)
            }
        }
    }

### Unieważnienie

Możemy wprowadzić unieważnienie, tworząc spedytora za pomocą przełącznika kill.

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

2) Tworzymy port do nasłuchiwania wywołań metod i kanał `forwardFlag`, aby zapisać czy przekazywać wiadomości.

3) Zwracamy kanał, na którym klienci wysyłają żądania i kanał, na który wysyłany jest sygnał zabicia (kill).

4) Ustawiamy stan początkowy `forwardFlag` na prawda.

5-10) Czytamy w arbitralnej krótce części wiadomości i otrzymujemy i zastępujemy wartość flagi. Jeśli flaga jest prawdziwa, przekazujemy wiadomość krotki do `target`.

11-13) Jeśli wiadomość jest kiedykolwiek wysyłana na kanale `kill`, ustawiamy ` forwardFlag` na fałsz. Proces przesyłania dalej przestaje przekazywać wiadomości.

### Kompozycja

Łącząc spedytora atenuacyjnego z spedytorem unieważnienia, otrzymujemy obie funkcje:

    new ret in {
        MakeGetForwarder(target, ret) |
        for (pair <- ret) {
            match pair with [getOnly, kill] => {
                MakeRevokableForwarder(getOnly, ret) |
                for (revokableGetOnly <- ret) {
                    // Wyślij revokableGetOnly zamiast celu
                    // Zawieś na kill, aby móc później unieważnić
                }
            }
        }
    }

### Spedytor do zapisywania

Spedytor do zapisywania może zapisywać wszystkie wiadomości wysłane na kanale, przekazując je do drugiego kanału.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new port in {
            ret(port) |
            contract port(tuple) {
                target(tuple) |
                logger(tuple)
            }
        }
    }

### Odpowiedzialność

Załóżmy, że Alicja ma kanał i chce zapisywać dostęp Boba do niego. Bob chciałby przekazać wykorzystanie tego kanału do Caroli i zapisać jej dostęp. Każdy z ich ma możliwość skonstruowania własnego spedytora do zapisywania wokół kanału, który otrzymali. Alice przekaże odpowiedzialnośc Bobi za to, co robi Carol.

### Uszczelnianie i rozszczelnianie

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

Para do Uszczelniania/rozszczelniania zapewnia taką samą funkcjonalność, jak klucze publiczne, ale bez kryptografii. Jest to po prostu atenuacja opisanej powyżej kontroli płaszcza. Ten wzór może być użyty do podpisania czegoś w imieniu użytkownika. W samouczku blockchain Rholanga zobaczymy, że działa ona nawet na blockchainie, ponieważ nie ma żadnych sekretów do przechowywania, tylko nazwy nie do podrobienia które pozostają niedostępne.

### Uwaga na wysyłanie attenuatorów

Podstawową zasadą, o której należy pamiętać o procesów RChain, jest metoda podobna do tradycyjnych aplikacji internetowych: dowolny kod wysyłany do innej partii może zostać zdemontowany. Od końca lat dziewięćdziesiątych, kiedy kupowanie rzeczy przez Internet stało się możliwe,  [istniały platformy handlu elektronicznego](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/), w których platforma polegała na przeglądarkach użytkowników, aby wysłać prawidłową cenę przedmiotu do niej. Autorzy nie myśleli o tym, że użytkownik może otworzyć narzędzia deweloperskie i zmienić cenę, zanim zostanie odesłana. Właściwym sposobem na zbudowanie platformy handlu elektronicznego jest przechowywanie cen na serwerze i sprawdzanie ich na tym serwerze.

Załóżmy, że Bob jest gotów uruchomić kod dla Alicji; ma umowę, która mówi coś w rodzaju: "Weź proces z tego kanału i uruchom go".

    for (p <- x) { *p }

To jest tak, jak przeglądarka internetowa skłonna do uruchomienia kodu JavaScript, który pobiera ze strony internetowej. Jeśli Alice przesyła Bobowi spedytor attenuacji, Bob może użyć produkcje dopasowywania wzorców w Rholang, aby zdemontować proces i uzyskać dostęp do bazowego zasobu. Zamiast tego, podobnie jak w przypadku handlu elektronicznego, Alicja powinna wysyłać tylko kod, który przekazuje żądania do jej własnych procesów i dokonać atenuacji tam.

## Konkluzja

RChain jest językiem zaprojektowanym do użycia na blockchain, ale nie wspominaliśmy nic o węzłach, przestrzeniach nazw, portfelach, Rev i phlogiston, strukturze sieci lub Casper. Nadchodzący dokument zajmie się wszystkimi tymi kwestiami i nie tylko.

Mamy nadzieję, że powyższe przykłady wzbudzają chęć napisania więcej kodu i wykazują łatwość wyrażania współbieżnych projektów.