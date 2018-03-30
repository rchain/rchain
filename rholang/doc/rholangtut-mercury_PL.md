# Instrukcja Rholanga

Rholang to nowy język programowania przeznaczony do stosowania w systemach rozproszonych. Jak wszystkie nowonarodzone rzeczy, szybko się zmienia i rośnie; ten dokument opisuje składnię, która będzie używana w wydaniu Mercury.

Rholang jest "zorientowany na proces": wszystkie obliczenia odbywają się za pomocą przekazywania komunikatów. Wiadomości są przekazywane na "kanałach", które są raczej kolejkami wiadomości, ale zachowują się jak zestawy, a nie kolejki. Rholang jest całkowicie asynchroniczny, w tym sensie, że podczas gdy możesz czytać wiadomość z kanału, a następnie coś z nią zrobić, nie możesz wysłać wiadomości, a następnie zrobić coś po otrzymaniu - przynajmniej nie bez wyraźnego czekanie na wiadomość potwierdzającą z odbiornika.

## Kontrakty, refleksje i przesyłanie danyc

    1 contract @"HelloWorld"(system) = {
    2     system!("print", "Hello, world!")
    3 }

1) W Internecie serwery mają adresy IP. Usługa DNS (Domain Name Service) mapuje łańcuchy alfanumeryczne na liczby, jak książka telefoniczna. Zamiast używać liczb lub łancuchów znaków, Rholang jest "refleksyjny": wszystkie kanały są nazwane serializowanym procesem. Wszystkie serializacje procesów rozpoczynają się od `@`. Ta umowa nasłuchuje wiadomości wysyłanych na kanale nazwanym przez serializację procesu łancucha zanków ` "HelloWorld" `. Aby być zwięzłymi, mówimy: "Ta umowa nasłuchuje nazwy" `@ "HelloWorld" `." 

Kontrakt deklaruje API, dzięki któremu inne procesy mogą z nim współdziałać. Kontrakty na najwyższym poziomie mają ten sam API: mają jeden argument, kanał dla procesu `system`. Proces `system` zawiera nazwy wszystkich kanałów, które mogą powodować efekty uboczne w węźle. Jest to wbudowany proces, który nasłuchuje komunikatów składających się z nazwy metody i niektórych argumentów.

2) Operator punktu wykrzyknika przesyła wiadomość po swojej prawej stronie nad kanałem po lewej stronie. Każda wiadomość to krótka nazw. Jeśli wyślemy proces zamiast nazwy, zostanie on automatycznie serializowany do nazwy.

W tym przypadku wysyłamy wiadomośc składającą się z dwóch procesów: łancuch znaków  `" print "` i łancuch znaków `" Hello, world! "`. Proces `system` jest wbudowanym procesem, który nasłuchuje komunikatów składających się z nazwy metody i niektórych argumentów; w tym przypadku proces adbija drugi argument na standardowe wyjście.

## Nowe kanały, odbieranie danych i wzorc

    1 contract @"HelloAgain"(system) = new chan in {
    2     chan!("Hello again, world!") |
    3     for (@text <- chan) system!("print", text)
    4 }

1) Aby utworzyć nowy, prywatny kanał, używamy konstrukcji `new ... in`. Żaden inny proces nie może wysyłać ani odbierać wiadomości za pośrednictwem tego kanału, chyba że wyraźnie wyślemy ten kanał do innego procesu.

2) Wysyłamy łancuch znaków `"Hello again, world!"` na nowym kanale.

3) Słuchamy na nowym kanale dla wiadomości. Operacja `for` jest zablokowana dopóki nie pojawi się wiadomośc na kanale `chan`.

Język wzorców (znany również jako "typy przestrzenne") obejmuje język procesów i nazw. Każdy proces z wolnymi zmiennymi jest wzorcem pasującym do procesu o tej samej strukturze; wolne zmienne wiążą się z podprocesami w tej pozycji. W linii 3 oczekujemy krótke z jednym elementem. Ten element jest serializacją jakiegoś procesu, a zatem zaczyna się od `@`. Ponieważ wysłaliśmy łancuch znaków w linii 2, wolna zmienna `text` zostaje powiązana z tym procesem łancucha znaków. Wreszcie, linia 3 przekazuje ten ciąg do procesu `system` do wydrukowania.

Później zbadamy więcej cechów wzorów.

## Stan zmienny, replikacja i wybranie

     1 contract @"CellDemo"(system) = new MakeCell in {
     2     // Tworzy pojedynczą komórkę, w której możesz przechowywać wartościs
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
    13     // Używanie komórki.
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

1) Tworzymy nowy kanał MakeCell, a następnie używamy go na linii 3 jako nazwy umowy wewnętrznej. Żaden proces inny niż kod wewnątrz kontraktu `@"CellDemo"` może go wywołać.

3) Umowa `MakeCell` przyjmuje trzy argumenty. Pierwszy argument to proces, wartość początkowa, która ma być przechowywana w komórce. Drugi i trzeci argument to kanały, przez które komórka odbierze żądania pobrania i ustawienia wartości. Ci, którzy znają C ++, mogą myśleć o kanałach w przybliżeniu do pointerów w tym sensie, że wskaźnik jest serializowalnym typem danych, który oznacza lokacje. Operator `@` w tym kontekście jest mniej więcej równoważny z `& ` oznaczającym przekazywanie-przez-referencję; zmienna `init` zostaje powiązana z procesem zamiast z kanałem. W C++, aby przekonwertować wskaźnik na referencję, używa się operatora `*`; podobnie, w Rholangu, aby przekonwertować kanał na proces, używamy operatora `*`. Ponieważ możemy wysyłać procesy tylko przez kanały, operator `*` jest bardzo często używany; zobacz nieparzyste linie w przykładzie użycia.

Aby zapisać wartość, tworzymy nowy kanał. Ten kanał będzie miał najwyżej jedną wiadomość zawierającą bieżącą wartość komórki.

4) Przed tą linią nie ma żadnych wiadomości na kanale `valueStore`. Po wysłaniu wartości początkowej jest to jedyna wartość na tym kanale.

5) Równolegle z 4, próbujemy odczytać z `valueStore`. Strzałka z podwójnumi łodygami mówi, że gdy dostaniemy wiadomość na tym kanale, powinniśmy utworzyć kopię procesu natychmiast po `for` i od razu zacząć czytać na tym kanale.

Gdy wiadomość stanie się dostępna, wiążemy zmienną `value` z procesem w wiadomości.

6-8) Słowo kluczowe `select` w linii 6 oznacza, że ​​tylko jedno z oddziałów na liniach 7 lub 8 będzie mogło kontynuować. W tym momencie nie ma wiadomości oczekującej na kanale `valueStore`.

Jeśli na kanale `get` pojawi się wiadomośc, to oddział na linii 7 może działać. Zmienna `ack` zostaje powiązana z wiadomoscią , a następnie równolegle występują dwie rzeczy: wartość, którą odczytamy, zostaje wysłana ponownie w `valueStore`, a także na kanalu `ack`.

Wiadomości to krótki imion. Wszystkie wiadomości, które widzieliśmy do tej pory, miały arność 1, ale tutaj na linii 8 czekamy na komunikat z arnośćą 2. Pierwsza część wzorca wiąże zmienną `newValue` z pierwszą częścią wiadomości, podczas gdy druga część wzorca wiąże zmienną `ack` z kanałem. Zamiast wysyłać `value` na `valueStore`, tak jak zrobiliśmy w linii 7, wysyłamy `newValue`.

Również w linii 8 wysyłamy wiadomość o arnośći 0. W przykładzie użycia linie 20 i 22 używają znaku podkreślenia, aby odebrać tę pustą wiadomość i ją odrzucić.

9) W tym momencie na kanale `valueStore` jest dokładnie jedna wiadomośc.

13-31) Kod użycia demonstruje utworzenie komórki, przypisanie wartości początkowej 123, pobranie i wydrukowanie tej wartości, ustawienie wartości na 456, a następnie pobranie i wydrukowanie tej wartości.

Zwróć uwagę na głębokie warstwy wywołania zwrotnego. Rholang został zaprojektowany, aby obliczenia równoległe były naturalne do wyrażania; w związku z tym zależnośći danych ukrytych w sekwencjach w innych językach muszą być wyraźne.

## Iteracja 

    1 contract @"IterateDemo"(system) = new chan in {
    2     [1,2,3].iterate(chan) |
    3     for (@num, ack << chan) system!("print", num, *ack)
    4 }

2) Nawiasy wskazują listę. Listy są zmienne, a krotki, oznaczone parentezami, nie. Niektóre procesy, takie jak te wspierane przez obiekty Java, mają metody; w tym przypadku metoda `iterate` jest wywoływana z kanałem do iteracji.

3) Operator `<<` wskazuje "wysyłanie sekwencyjne" ,lub innymi słowami, że wiadomości od `chan` wymagają potwierdzenia przed wysłaniem nowych wiadomości.

Metoda `print` procesu systemowego może przyjmować jeden lub dwa argumenty. W przypadku dwóch parametrów drugim argumentem jest kanał, na którym zostanie wysłana wiadomość potwierdzająca po zakończeniu drukowania. Metoda `iterate` otrzyma to potwierdzenie i wyśle następną wiadomość. Po zakończeniu iteracji listy linia 3 ewoluuje się w proces `Nil`, który nic nie robi i zostaje pozbierany przez zbieranie nieużytków.

## Dopasowanie wzórów i parametry odpoczynku

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
    33     // Użycie
    34     new ret in {
    35         MakeCoatCheck(ret) |
    36         for (cc <- ret) {
    37             // Tworzy nową komórkę z początkową wartością 0e 0
    38             cc!("new", *ret, 0) |
    39             for (ticket <- ret) {
    40                 // Ustawia komórkę na 1
    41                 cc!("set", *ret, *ticket, 1) |
    42                 for (ack <- ret) {
    43                     // Czyta wartość
    44                     cc!("get", *ret, *ticket) |
    45                     for (@storedValue <- ret) {
    46                         // Drukuje 1
    47                         system!("print", storedValue)
    48                     }
    49                 }
    50             }
    51         }
    52     }
    53 }

2) Jeden wzorzec projektowy, stosowany w powyższej umowy MakeCell, polega na odbieraniu od dzwoniącego kanału dla każdego innego elementu funkcjonalności, który jest zapewniony przez proces. Programista obiektowy móg by powiedzieć, że MakeCell wymaga od wywołującego podania kanału dla każdej metody. Dopasowania są próbowane w kolejności, w jakiej pojawiają się w kodzie; jeśli nie występuje dopasowanie, blok `match` ocenia do procesu `Nil`.

3) MakeCoatCheck używa bardziej obiektowo-zorientowanego podejścia. Konstrukcja `(in, out): iopair` pozwala nam stworzyć sprzężoną parę kanałów. Jest to błąd typu wysyłac wiadomości przez kanał `in` lub odbierać wiadomości przez kanał `out`. Jednak każda wiadomość wysłana przez kanał `out` może być odbierana przez kanał `in`. Dzięki temu możemy zwrócić kanał `out`, na którym można wykonać "wywołania metody", bez umożliwienia innym procesom przechwytywania żądań przeznaczonych dla naszego procesu.

Nowy kanał `table` będzie używany do tworzenia kanałów do użytku wewnętrznego.

5) Wielokrotnie czytamy tu w wiadomościach o każdej arnośći większej lub równej dwóm. Zmienna `method` zostaje powiązana z pierwszym argumentem; spodziewamy się łańcucha znaków, który nadaje nazwę tej metodzie, podobnie jak robi proces systemowy. Zmienna `ack` zostaje przywiązana do kanału, na którym wyślemy dowolny wynik wywołania metody. Zmienna "reszta" zostaje powiązana z krotką zawierającą resztę części wiadomości.

6) Konstrukcja `match ... case` pozwala nam dopasować wzór na strukturze procesu. Używamy jej do wysyłania wiadomości.

7-12) Jeśli `method` jest łańcuchem znaków `"new"`, to zakładamy że w linii 8  `rest` będzie krotką z jednym elementem, wartością początkową. Tworzymy kanał `ticket` i zwracamy go za pomocą kanału `ack`. Łączymy również procesy `*ticket` z ` *table` w nowy proces, a następnie wyprowadzamy z nich nazwę kanału. Ponieważ tylko my mamy dostęp do `table`, tylko my możemy manipulować danymi przechowywanymi na kanałach skonstruowanych takim sposobem. Kanał `ticket` zachowuje się jak bilet odkupny na sprawdzany płaszcz, i kanał `@(*ticket | *table` jest zapisem tabeli wskazanym przez ten klucz.

13-19) Jeśli `method` jest łańcuchem znaków `"get"`, to przyjmujemy w linii 14, że `rest` będzie krotką z jednym elementem, szczególnym biletem do elementu do pobrania przez sztuke. Operator `<!` odczytuje wartość z tabeli i natychmiast ją odkłada; to jest,

    for (y <! x) { P }

to lukier składniowy dla

    for (y <- x) { x!(*y) | P }.

20-27) Jeśli `method` jest łańcuchem znaków `"set"`, to w linii 21 przyjmujemy, że `rest` będzie krotką z dwoma elementami: kluczem i nową wartością. Linia 22 odrzuca aktualną wartość na tym bilecie, linia 23 wysyła nową wartość, a linia 24 informuje, że jest to zrobione.

## Obsługa błędów

     1 for (@info, ret, err <- channel) {
     2     // Zwróć wynik na ret lub błąd na er
     3 } |
     4 select {
     5     result <- ret => {
     6         // Wynik procesu
     7     }
     8     // Wiadomości na err, które nie pasują do tego wzorca
     9     // nie są tutaj przechwytywane.
    10     @"TypeError", msg <- err => {
    11         // Błąd typu uchwytu
    12     }
    13 }

1-3) Możemy określić wiele kanałów, na których dane mogą być odesłane do klienta.

4) W tym kontekście `select` zachowuje się jak `try` w innych językach. Tylko jedno z dostarczeń w liniach 5 i 10 będzie kontynuowane; ścigają się, aby zobaczyć, które z nich otrzyma wiadomość pierwsze. Jeśli linie 1-3 mają niezmiennik, że albo wynik zostanie wysłany na `ret` lub błąd na `err`, wtedy nie będzie wyścigu. Jeśli, z drugiej strony, chcemy wysłać zarówno wynik, jak i błąd, powinniśmy użyć `for` zamiast:

    for (@info, ret, err <- channel) {
        // Zwróć wynik na ret lub błąd na 
    } |
    for(result <- ret) {
        // Wynik procesu
    } |
    for(@"TypeError", msg <- err) {
        // Błąd typu uchwytu
    }

10)Wzór tutaj jest bardziej skomplikowany niż te, które widzieliśmy wcześniej. Tutaj określamy, że chcemy tylko wiadomości o dwóch nazwach, a pierwsza nazwa powinna być serializacją łancuchu znaków `"TypeError"`. Jeśli jest coś innego, to rozgałęzienie nie będzie kontynuowane.

## Ucztująci filozofowie i deadlock

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

Problem ucztujących filozofów ma dwóch filozofów, którzy dzielą tylko jeden komplet sztućców. Filozof 1 siedzi po wschodniej stronie stołu, podczas gdy filozof 2 siedzi na zachodniej stronie. Każdy potrzebuje zarówno noża, jak i widelca do jedzenia. Każdy odmawia opuszczenia naczynia, dopóki nie użyje obu do ugryzienia. Jeśli obaj filozofowie sięgną najpierw po naczynie po ich prawej stronie, obaj umrą z głodu: Filozof 1dostaje nóż, philosopher2 dostaje widelec i nikt ich nie puszcza.

Oto, jak rozwiązać problem:

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

4, 7) Operator łączenia, oznaczony średnikiem `;`, deklaruje, że kontynuacja powinna być kontynuowana tylko wtedy, gdy na każdym z kanałów jest dostępna wiadomośc, zapobiegając powyższemu deadlocka.

## Wbudowane typy, stałe wysyłanie, logiczne spójniki i filtrowanie

Widzieliśmy, że wzorce, których możemy użyć w konstrukcji `match` lub `for` zawierają procesy z wolnymi zmiennymi. Możemy również użyć wzorów opisujących wbudowane procesy. Wzór `Integer` opisuje wszystkie 32-bitowe liczby całkowite ze znakiem; podobnie do `Double`, `String` i `Boolean`.

Możemy łączyć wzorce za pomocą logicznych spójników AND, OR i NOT, oznaczonych odpowiednio symbolami `&&`, `||` i  `~`.

    for (@(x && Integer) <- y) { P }

Ten proces wiąże zmienną procesową `x` z wiadomością odebraną na `y`, ale jednocześnie nalega, że ​​`x` jest liczbą całkowitą.

Jest taka amerykańska zagadka, która mówi: "Jak robisz piętnaście centów w drobnych, gdy jedna moneta nie jest nickel, a druga nie jest dime?" Nikiel wart jest pięć centów, a dime jest wart dziesięć. Załóżmy, że mamy następujące wiadomości wysyłane na `coins`, kodujące dostępne rodzaje amerykańskich monet o wartości poniżej piętnastu centów:

    coins!!(1) | coin!!(5) | coin!!(10)

Operator `!!` oznacza, że wiadomości powinny trwale pozostać na kanale i nie powinny być skonsumowane, gdy są odbierane przez konstrukcję `for`.

    new x in { x!!("Hi there!") | for (msg <= x) { system!("print", msg) } }

Powyższy proces wyświetli "Cześć!" tak długo, jak działa wirtualna maszyna.

Możemy zakodować zagadkę jako

    for (@(x && ~5) <- coins; @(y && ~10) <- coins if x+y == 15) {
        system!("print", (x, y))
    }

Pierwszy wzorzec `@(x && ~5)` dopasuje dowolną wiadomość na `coins`, która nie jest 5 i przywiąże do niej `x`; podobnie, drugi wzorzec `@(y && ~10)` dopasuje dowolną wiadomość na `coins` która nie jest 10 i będzie wiązała `y` z nią. Klauzula `if` w konstrukcji `for` zezwala tylko na to, aby dopasowanie było kontynuowane, jeśli formuła po jej prawej stronie sprawdzi `true`; w tym przypadku nic nie zostanie wydrukowane, chyba że wartości sumują się do 15.

Odpowiedzią na zagadkę jest "1 dime i 1 nikiel", ponieważ ani dime nie jest niklem, a nikiel nie jest dimem. Zmienna "x" zwiąży do 10, a zmienna "y" zwiąży do 5, i `(10, 5)` zostanie wydrukowane.

## Bezpieczne wzorce projektowe

W tej sekcji opisujemy kilka wzorców projektowych. Wzory te są zaadaptowane z [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf) Marca Stieglera.

### Aspekty

W umowie MakeCell klient udostępnia dwa kanały, jeden do uzyskania wartości i drugi do ustawienia jej. Jeśli klient przekazuje tylko kanał `get` do innego procesu, proces ten efektywnie ma widok tylko-do-odczytu  tej komórki.

Kanały takie jak `get` i `set` są nazywane "aspektami" procesu.  Zawierają one autorytet do wykonania działania. Jeśli kanał `set` jest kanałem publicznym takim jak `@"Foo"`, wówczas każdy, kto może nauczyć się lub nawet odgadnąć łańcuch znaków `"Foo"` ma uprawnienia do ustawienia wartości komórki. Z drugiej strony, jeśli kanał `set` został utworzony za pomocą operatora `new` , wówczas żaden inny proces nie może zbudować kanału `set`; musi zostać przekazany do procesu bezpośrednio, aby proces mógł go użyć. 

Zauważ, że jeśli `get` i` set` nie są tworzone jako połówki iopairs, to posiadanie tych kanałów jest także uprawnieniem do przechwytywania wiadomości wysyłanych do komórki:

    for (ret <- get) { P } | 
    for (ret <- get) { Q } | 
    get!(*ack)

Ten termin ma dwa procesy nasłuchujące na kanale `get` i pojedynczą wiadomość wysłaną przez `get`. Tylko jeden z dwóch procesów będzie mógł odebrać wiadomość.

Odbierając kanały od klienta w celu uzyskania i ustawienia, umowa MakeCell pozostawia decyzje o tym, jak publiczne są te kanały dla klienta. Z drugiej strony, umowa MakeCellFactory konstruuje własne kanały i zwraca je klientowi, dzięki czemu jest w stanie egzekwować gwarancje prywatności.

### Atenuacja spedytorów

W umowie MakeCellFactory jest tylko jeden kanał, a komunikaty są wysyłane wewnętrznie. Aby uzyskać taki sam efekt jak aspekt tylko-do-odczytu, możemy utworzyć proces spedytora który po prostu ignoruje wszystkie wiadomości, których nie chce przekazywać dalej. Poniższy kontrakt przekazuje jedynie metodę "get".

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

### Unieważnienie

Możemy wprowadzić unieważnienie, tworząc spedytora za pomocą przełącznika kill.

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

2) Tworzymy iopair dla metody wysyłki i kanał `forwardFlag`, aby zapisać czy przekazywać wiadomości.

3) Zwracamy kanał, na którym klienci wysyłają żądania i kanał, na który wysyłany jest sygnał zabicia (kill).

4) Ustawiamy stan początkowy `forwardFlag` na prawda.

5-13) Czytamy w arbitralnej krotce części wiadomości i otrzymujemy i zastępujemy wartość flagi. Jeśli flaga jest prawdziwa, przekazujemy wiadomość krotki do `target`.

14-15) Jeśli wiadomość jest kiedykolwiek wysyłana na kanale `kill`, ustawiamy `forwardFlag` na fałsz, co zatrzymuje przekazywanie wiadomości.

### Kompozycja

Łącząc spedytora atenuacyjnego z spedytorem unieważnienia, otrzymujemy obie funkcje:

    new ret in {
        MakeGetForwarder(target, *ret) |
        for (@getOnly, kill <- ret) {
            MakeRevokableForwarder(getOnly, *ret) |
            for (@revokableGetOnly <- ret) {
                // Wyślij revokableGetOnly zamiast celu
                // Zawieś na kill, aby móc później unieważnić
            }
        }
    }

### Spedytor do zapisywania

Spedytor do zapisywania może zapisywać wszystkie wiadomości wysłane na kanale, przekazując je do drugiego kanału.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new (portIn, portOut):iopair in {
            ret!(*portOut) |
            for (...@rest <= portIn) {
                target!(...rest) |
                logger!(...rest)
            }
        }
    }

### Odpowiedzialność

Załóżmy, że Alicja ma kanał i chce zapisywać dostęp Boba do niego. Bob chciałby przekazać wykorzystanie tego kanału do Caroli i zapisać jej dostęp. Każdy z ich ma możliwość skonstruowania własnego spedytora do zapisywania wokół kanału, który otrzymali. Alice przekaże odpowiedzialnośc Bobi za to, co robi Carol.

### Uszczelnianie i rozszczelnianie

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

Para do Uszczelniania/rozszczelniania zapewnia taką samą funkcjonalność, jak klucze publiczne, ale bez kryptografii. Jest to po prostu atenuacja opisanej powyżej kontroli płaszcza. Ten wzór może być użyty do podpisania czegoś w imieniu użytkownika. W samouczku blockchain Rholanga zobaczymy, że działa ona nawet na blockchainie, ponieważ nie ma żadnych sekretów do przechowywania, tylko nazwy nie do podrobienia które pozostają niedostępne.

### Uwaga na wysyłanie attenuatorów

Podstawową zasadą, o której należy pamiętać o procesów RChain, jest metoda podobna do tradycyjnych aplikacji internetowych: dowolny kod wysyłany do innej partii może zostać zdemontowany. Od końca lat dziewięćdziesiątych, kiedy kupowanie rzeczy przez Internet stało się możliwe,  [istniały platformy handlu elektronicznego](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/), w których platforma polegała na przeglądarkach użytkowników, aby wysłać prawidłową cenę przedmiotu do niej. Autorzy nie myśleli o tym, że użytkownik może otworzyć narzędzia deweloperskie i zmienić cenę, zanim zostanie odesłana. Właściwym sposobem na zbudowanie platformy handlu elektronicznego jest przechowywanie cen na serwerze i sprawdzanie ich na tym serwerze.

Załóżmy, że Bob jest gotów uruchomić kod dla Alicji; ma umowę, która mówi coś w rodzaju: "Weź proces z tego kanału i uruchom go".

    for (@P <- x) { P }

To jest tak, jak przeglądarka internetowa skłonna do uruchomienia kodu JavaScript, który pobiera ze strony internetowej. Jeśli Alice przesyła Bobowi spedytor attenuacji, Bob może użyć produkcje dopasowywania wzorców w Rholang, aby zdemontować proces i uzyskać dostęp do bazowego zasobu. Zamiast tego, podobnie jak w przypadku handlu elektronicznego, Alicja powinna wysyłać tylko kod, który przekazuje żądania do jej własnych procesów i dokonać atenuacji tam.

## Konkluzja

RChain jest językiem zaprojektowanym do użycia na blockchain, ale nie wspominaliśmy nic o węzłach, przestrzeniach nazw, portfelach, Rev i phlogiston, strukturze sieci lub Casper. Nadchodzący dokument zajmie się wszystkimi tymi kwestiami i nie tylko.

Mamy nadzieję, że powyższe przykłady wzbudzają chęć napisania więcej kodu i wykazują łatwość wyrażania współbieżnych projektów.