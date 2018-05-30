# Instrukcja dla Rholang

Rholang to nowy język programowania przeznaczony do stosowania w systemach rozproszonych. Jak wszystkie nowonarodzone rzeczy, szybko się zmienia i rośnie; ten dokument opisuje składnię, która będzie używana w wprowadzeniu RNode-0.2.

Rholang jest "zorientowany na proces": wszystkie obliczenia odbywają się za pomocą przekazywania komunikatów. Wiadomości są przekazywane na "kanałach", które są kolejkami wiadomości, ale zachowują się jak zestawy, a nie kolejki. Rholang jest całkowicie asynchroniczny, w tym sensie, że podczas gdy możesz czytać wiadomość z kanału, a następnie coś z nią zrobić, nie możesz wysłać wiadomości, a następnie zrobić coś po otrzymaniu --- przynajmniej nie bez wyraźnego czekania na wiadomość potwierdzającą z odbiornika. Należy zauważyć, że w całym dokumencie słowa "nazwa" i "kanał" są używane zamiennie. Dzieje się tak dlatego, że w rho-calculus (na którym opiera się Rholang) terminu nazwa jest używany, jednak ponieważ możesz wysyłać i odbierać informacje przez nazwy, semantycznie są one jak kanały.

## Kontrakty i wysyłanie danych

    1 new helloWorld in {
    2   contract helloWorld(name) = {
    3     "Hello, ".display(name, "!\n")
    4   } |
    5   helloWorld("Joe")
    6 }

1) Program Rholang jest pojedynczym procesem z jedną lub kilkoma równoległymi kompozycjami. Ten proces rozpoczyna się od utworzenia umowy o nazwie `@"HelloWorld"`. Produkcja `contract` (umowy) tworzy proces, który spawnuje kopię swojej treści za każdym razem, gdy otrzymuje wiadomość. Zauważ, że w Rholang wszystkie procesy można "zacytować" przy pomocy `@`  w celu utworzenia kanału. Ciągi są poprostu specjalnymi procesami, więc możemy zacytować dowolny ciąg aby utworzyć kanał.

2) Na kanale zwrotnym wysyłamy proces, który jest ciągiem `"Hello, World!"`.

4) Aby utworzyć nowy, prywatny kanał, używamy konstrukcji `new ... in`. Żaden inny proces nie może wysyłać ani odbierać wiadomości za 
pośrednictwem tego kanału, chyba że wyraźnie wyślemy ten kanał do tego innego procesu.

5) Wysyłamy kanał `myChannel` do umowy na `@"HelloWorld"`. Operator `*` "kończy cytat" kanału, aby uzyskać jego podstawowy proces. W Rholang możesz wysyłać tylko procesy przez kanały; nie możesz wysyłać kanałów przez kanały. Dlatego używamy  `*` , aby przekształcić kanał prywatny w proces przed wysłaniem.

## Odbieranie danych

    1 contract @"HelloAgain"(_) = {
    2   new chan in {
    3     chan!("Hello again, world!") |
    4     for (@text <- chan) { Nil }
    5   }
    6 } | @"HelloAgain"!(Nil)

1) Umowy przyjmują co najmniej jeden parametr, ale możemy go wyrzucić, przez wiązanie go z symbolem wieloznacznym  `_`.

2) Tworzymy nowy kanał `chan`.

3) Wysyłamy proces ciąga `"Hello again, world!"` na nowym kanału.

4) Słuchamy na nowym kanału dla pojedynczej wiadomości. Operacja `for`, jest zablokowana dopóki się pojawi wiadomośc na kanału ` chan`. W Rholang możesz tylko odbierać nazwy na kanałach (zauważ, że różni się to od wysyłania!). Wiązanie po lewej stronie od `<-` - w `for` jest w rzeczywistości wzorem nazwy. W tym przykładzie wzór to `@text`, co oznacza, że otrzymywana nazwa jest procesem zacytowanym i że chcemy powiązać ten proces z wolną zmienną `text` . Operacja `for` jest tak jak umowa, z tą różnicą, że odczytuje tylko jedną wiadomość, a następnie staje się jej treścią zamiast rozgałęziać kopię jej treści dla każdej wiadomości. W tym przypadku wybieramy nic nie robić w treści `for` przez zwykłym przekształcaniem jej do zatrzymanego procesu `Nil`, jednak w zasadzie chcielibyśmy troche dalej 
kontynuować przetwarzanie `text` zawartego w `chan`.

## Stan zmienny

     1 new MakeCell in {
     2   // Tworzy pojedynczą komórkę, w której możesz przechowywać wartości
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
    18   // Używanie komórki.
    19   new myGet, mySet in {
    20     MakeCell!(123, *myGet, *mySet) |
    21     new ack in {
    22       myGet!(*ack) |
    23       for (@result <- ack) {
    24         //Rezultat teraz zawiera wartość 123
    25         mySet!(456, *ack) |
    26         for (_ <- ack) {
    27           myGet!(*ack) |
    28           for (@result <- ack) {
    29             //Rezultat teraz zawiera wartość 456
    30             Nil
    31           }
    32         }
    33       }
    34     }
    35   }
    36 }

1) Tworzymy nowy kanał MakeCell, a następnie używamy go na linii 3 jako nazwe umowy wewnętrznej. Żaden proces inny niż kod w tym zakresie leksykalnym nie może go wywołać.

3) Umowa `MakeCell` przyjmuje trzy argumenty. Pierwszy argument to wartość początkowa, która ma być przechowywana w komórce. Drugi i trzeci argument to kanały, przez które komórka odbierze żądania pobrania i ustawienia wartości.  Zauważ, że chcemy aby pierwszy argument był procesem, a drugi i trzeci były nazwami, ale nazwy są zawsze odbierane przez kanały, więc musimy uczynić pierwszy argument jako wzór zaczynający się od `@`, aby wskazać, że nazwa, którą otrzymujemy jako pierwszy argument jest procesem cytowanym i jest to proces, którego chcemy powiązać ze zmienną.

4) Aby przechować wartość, tworzymy nowy kanał. Ten kanał będzie miał najwyżej jedną wiadomość zawierającą obecną wartość komórki.

5) Przed tą linią nie ma żadnych wiadomości na kanale `valueStore`. Po wysłaniu wartości początkowej jest to jedyna wartość na tym kanale.

6) Ustawiamy umowę, która nasłuchiwa na kanale `get`. Za każdym razem, gdy wiadomość zostanie wysłana na `get`, treść umowy zostanie wykonana.

7) Blokujemy, dopóki nie otrzymamy jednej wiadomości z kanału `valueStore`. Ponieważ w `ValueStore` jest co najwyżej jedna wiadomość oczekująca, czytanie wiadomości zachowuje się podobnie jak przy uzyskiwaniu zamka.

8) Ponownie wysyłamy bieżącą wartość od `valueStore`, pozwalając na przetwarzanie innych wiadomości (odemknając zamka) i  wysyłamy bieżącą wartość do klienta na kanale ` ack`.

11) Równolegle z umową `get` uruchamiamy umowe która odsłuchuje na `set`.

12) Blokujemy, dopóki sie pojawi wiadomośc na `valueStore`, a następnie ją odczytamy. Odrzucamy wiadomość, którą odczytalizmy.

13) Wysyłamy nową wartość do przechowania w `valueStore` i sygnalizujemy, że operacja została zakończona.

18-36) Kod użycia demonstruje utworzenie komórki, przypisanie wartości początkowej 123, pobranie tej wartości, ustawienie jej na 456, a następnie pobranie tej wartości.

Zwróć uwagę na głębokie warstwy wywołania zwrotnego. Rholang został zaprojektowany, aby obliczenia równoległe były naturalne do wyrażania; w związku z tym, zależnośći danych które są ukryte w sekwencjach w innych językach, muszą być wyraźne.

## Iteracja i dopasowanie

W poniższym kodzie, demonstrujemy przykład iteracji przez połączoną liste zaimplementowaną jako zagnieżdżone pary [orzeł, reszka].

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

3) Konstrukcja `match` umożliwia destrukturyzacje zmiennej przez dopasowanie do wzorca.

4) Jak `list` pasuje do wzorca pary orzeła/reszki to wykonujemy głowną treśc pętli.

5)Odczytujemy bieżący indeks z kanału.

6)Dodajemy 1 do indeksu i ponownie go zachowujemy.

7)Słowo kluczowe `match` również pozwala dopasowywać bardziej zkomplikowane expresje do wzorów.

8)Tu podwierdzamy że orzeł listy jest równy bieżącemu indeksowi i że zbieracz nadal jest równy z `true` i rekursywnie wywołuje pętlę ponownie.

13)Jeśli `list` nie pasuje do pary orzeła/reszki, to pętlą się kończy i zwracamy skumulowaną wartość.

16)Inicjowanie kanału indeksu.

17)Przywołanie pętlii z naszą początkową listą.

## Mapy

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
    26   // Użucie
    27   new ret, get, set in {
    28     MakeCoatCheck!(*ret) |
    29     for (cc <- ret) {
    30       // Tworzy nową konórke z początkową wartością 0
    31       cc!("new", 0, *ret) |
    32       for (ticket <- ret) {
    33         contract get(return) = { cc!("get", *ticket, *return) } |
    34         contract set(@value, return) = { cc!("set", *ticket, value, *return) } |
    35         
    36         get!(*ret) | for(@r <- ret) {
    37           //r jest równe 0
    38           for(_ <- ret){
    39             set!(1, *ret) | for(_ <- ret) {
    40               get!(*ret) | for(@r <- ret) {
    41                 //r jest równe 1
    42                 Nil
    43               }
    44             }
    45           }
    46         }
    47       }
    48     }
    49   }
    50 }

2) Jeden wzorzec projektowy, stosowany w powyższej umowie MakeCell, polega na odbieraniu od dzwoniącego, kanału dla każdego oddzielnego elementu funkcjonalności, który jest zapewniony przez proces. Programista obiektowy może powiedzieć, że MakeCell wymaga od dzwoniącego, podania kanału dla każdej metody.  MakeCoatCheck wykorzystuje podejście bardziej obiektowo oriętowane, jak zobaczymy.

3) Tworzymy kanał `port` aby interagować z sprawdzaniem płaszcza a także nazwe `table` która będzie używana do przechowywania/pobierania 
wartości w sprawdzaniu płaszcza.

4) Wysyłamy `port` do dzwoniącego, aby móg on interagować z sprawdaniem płaszcza.

5, 11, 17) Definiujemy różne metody, które można wywołać wysyłając wiadomość na `port`. Odbywa się to poprzez określenia wzajemnie wykluczających się wzorów, które wiadomośc na `port` może dopasować, z nazwą metody jako pierwszym elementem wiadomości , a argument(y) i kanał powrotny jako kolejnymi elementami. Użycie strzałki `<=` zamiast strzałki `<-` oznacza, że `for` są "replikowane". Daje im to takie samo zachowanie jak `contract`, tj. proces nasłuchujący na wiadomości na `port` nie ginie po utworzeniu instancji jego treści.

8) Korzystamy z możliwości cytowania dowolnego procesu w celu utworzenia nazwy aby utworzyć niepowtarzalną nazwę dla każdej wartości, która ma być przechowywana. Proces `*ticket | *table` jest tworzony przez równoległą kompozycję procesów produkowanych przez "od-cytowanie" nazw `ticket` i `table`. Proces ten można następnie zacytować w celu utworzenia niepowtarzalnej nazwy, która jest następnie używana do przechowywania wartości, przy wysyłania jej przez nazwę.

## Ucztująci filozofowie i deadlock

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

Problem ucztujących filozofów ma dwóch filozofów, którzy dzielą tylko jeden komplet sztućców. Filozof 1 siedzi po wschodniej stronie stołu, podczas gdy Filozof 2 siedzi na zachodniej stronie. Każdy potrzebuje zarówno noża, jak i widelca do jedzenia. Każdy odmawia opuszczenia naczynia, dopóki nie użyje obu do ugryzienia. Jeśli obaj filozofowie sięgną najpierw po naczynie po ich prawej stronie, obaj umrą z głodu: Filozof 1 dostaje nóż, Filozof 2 dostaje widelec i nikt ich nie puszcza.

Oto, jak rozwiązać problem:

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

4, 9) Operator łączenia, oznaczony średnikiem `;`, deklaruje, że kontynuacja powinna kontynuować tylko wtedy, gdy na każdym z kanałów jest dostępną wiadomośc równocześnie,  zapobiegając deadlocka powyżej.

## Bezpieczne wzorce projektowe

W tej sekcji opisujemy kilka wzorców projektowych. Wzory te są zaadaptowane z [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf) Marca Stieglera.

### Aspekty

W umowie MakeCell klient udostępnia dwa kanały, jeden do uzyskania wartości i drugi do ustawienia jej. Jeśli klient przekazuje tylko kanał `get` do innego procesu, proces ten efektywnie ma widok tylko-do-odczytu  tej komórki.

Kanały takie jak `get` i `set` są nazywane "aspektami" procesu.  Zawierają one autorytet do wykonania działania. Jeśli kanał `set` jest kanałem publicznym takim jak `@"Foo"`, wówczas każdy, kto może nauczyć się lub nawet odgadnąć łańcuch znaków `"Foo"` ma uprawnienia do ustawienia wartości komórki. Z drugiej strony, jeśli kanał `set` został utworzony za pomocą operatora `new` , wówczas żaden inny proces nie może zbudować kanału `set`; musi zostać przekazany do procesu bezpośrednio, aby proces mógł go użyć.

Zauważ, że posiadanie `get` i `set` jest także uprawnieniem do przechwytywania wiadomości wysyłanych do komórki:

    for (@ret <- get) { P } | 
    for (@ret <- get) { Q } | 
    get!(*ack)

Ten termin ma dwa procesy nasłuchujące na kanale `get` i pojedynczą wiadomość wysłaną przez `get`. Tylko jeden z dwóch procesów będzie mógł odebrać wiadomość.

Odbierając kanały od klienta w celu uzyskania i ustawienia, umowa MakeCell pozostawia decyzje o tym, jak publiczne są te kanały dla klienta. Z drugiej strony, umowa MakeCellFactory konstruuje własne kanały i zwraca je klientowi, dzięki czemu jest w stanie egzekwować gwarancje prywatności.

### Osłabiające przekaźniki

W umowie MakeCellFactory jest tylko jeden kanał, a komunikaty są wysyłane wewnętrznie. Aby uzyskać taki sam efekt jak aspekt tylko-do-odczytu, możemy utworzyć proces przekaźnika który po prostu ignoruje wszystkie wiadomości, których nie chce przekazywać dalej. Poniższy kontrakt przekazuje jedynie metodę "get".

    contract MakeGetForwarder(target, ret) = {
        new port in {
            ret!(*port) |
            contract port(method, @arg, ack) = {
                method == "get" match { true => target!("get", arg, *ack) }
            }
        }
    }

### Unieważnienie

Możemy wprowadzić unieważnienie, tworząc spedytora za pomocą przełącznika zabicia.

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

2) Tworzymy port do nasłuchiwania wywołań metod i kanał `forwardFlag`, aby zapisać czy przekazywać wiadomości.

3) Zwracamy kanał, na którym klienci wysyłają żądania i kanał, na który wysyłany jest sygnał zabicia (kill).

4) Ustawiamy stan początkowy `forwardFlag` na prawda.

5-10) Czytamy w arbitralnej wiadomości i otrzymujemy i zastępujemy wartość flagi. Jeśli flaga jest prawdziwa, przekazujemy wiadomość do `target`.

11-13) Jeśli wiadomość jest kiedykolwiek wysyłana na kanale `kill`, ustawiamy` forwardFlag` na fałsz. Proces przekazania wtedy przestaje przekazywać wiadomości.

### Kompozycja

Poprzez połączenia przekaźnika osłabiającego z przekaźnikiem unieważnienia, otrzymujemy obie funkcje:

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

### Przekaźnik do zapisywania

Przekaźnik do zapisywania może zapisywać wszystkie wiadomości wysłane na kanale, przekazując je do drugiego kanału.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new port in {
            ret!(*port) |
            contract port(msg) = {
                target!(msg) |
                logger!(msg)
            }
        }
    }

### Odpowiedzialność

Załóżmy, że Alicja ma kanał i chce zapisywać dostęp Boba do niego. Bob chciałby przekazać wykorzystanie tego kanału do Caroli i zapisać jej dostęp. Każdy z ich ma możliwość skonstruowania własnego spedytora do zapisywania wokół kanału, który otrzymali. Alice przekaże odpowiedzialnośc Bobi za to, co robi Carol.

### Uszczelnianie i rozszczelnianie

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

Para do uszczelniania/rozszczelniania zapewnia taką samą funkcjonalność, jak klucze publiczne, ale bez kryptografii. Jest to po prostu atenuacja opisanej powyżej kontroli płaszcza. Ten wzór może być użyty do podpisania czegoś w imieniu użytkownika. W samouczku blockchain Rholang zobaczymy, że działa ona nawet jako parą kluczy do podpisu/weryfikacji na blockhain, ponieważ nie ma żadnych sekretów do przechowywania, tylko nazwy których nie można podrobić, które muszą pozostać niedostępne.

### Uwaga na wysyłanie attenuatorów

Podstawowa zasada, o której należy pamiętać z procesami RChain, jest jedna podobna do tradycyjnych aplikacji internetowych: każdy kod wysyłany do innej partii może zostać zdemontowany. Od końca lat dziewięćdziesiątych, kiedy kupowanie rzeczy przez internet stało się możliwe,  [istniały platformy handlu elektronicznego](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/), w których platforma polegała na przeglądarkach użytkowników, aby wysłać prawidłową cenę produktu do niej. Autorzy nie myśleli o tym, że użytkownik może otworzyć narzędzia deweloperskie i zmienić cenę, zanim zostanie odesłana. Właściwym sposobem na zbudowanie platformy handlu elektronicznego jest przechowywanie cen na serwerze i sprawdzanie ich tam.

Załóżmy, że Bob chcę uruchomić kod dla Alicji; ma umowę, która brzmi jako: "Weź proces z tego kanału i uruchom go".

    for (p <- x) { *p }

To jest tak, jak przeglądarka internetowa skłonna do uruchomienia kodu JavaScript, którego pobiera ze strony internetowej. Jeśli Alice przesyła Bobowi przekaźnik attenuacji, Bob może użyć produkcje dopasowywania wzorców w Rholang, aby zdemontować proces i uzyskać dostęp do bazowego zasobu. Zamiast tego, podobnie jak w przypadku handlu elektronicznego, Alicja powinna wysyłać tylko kod, który przekazuje żądania do jej własnych procesów i dokonać atenuacji tam.

## Konkluzja

RChain jest językiem zaprojektowanym do użycia na blockchain, ale nie wspominaliśmy nic o węzłach, przestrzeniach nazw, portfelach, Rev i phlogiston, strukturze sieci lub Casper. Nadchodzący dokument zajmie się wszystkimi tymi kwestiami i nie tylko.

Mamy nadzieję, że powyższe przykłady wzbudzają chęć napisania więcej kodu i wykazują łatwość wyrażania współbieżnych projektów.
