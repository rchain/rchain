# Priručnik za Rholang 

Rholang je novi programski jezik dizajniran za uporabu u distribuiranim sustavima. Poput svih novorođenih, raste i brzo se mijenja; ovaj dokument opisuje sintaksu koja će se koristiti u izdanju RNode-0.3. 

Rholang je "procesno orijentiran": sve se izračunava pomoću propuštanja poruka. Poruke se prenose na "kanale", koje su prilično slične redovima, ali se ponašaju kao skupovi a ne redovi. 
Rholang je potpuno asinkron, u smislu da dok čitate poruku s kanala i činite nešto s njom, ne možete slati poruku i nešto činiti s njom jednom kad je primljena, barem ne bez potvrdne poruke primatelja. Imajte na umu da se u ovom dokumentu riječi "ime" i "kanal" koriste zamjenjivo. To je zato što je u rho-calculusu (na kojem se temelji Rholang) termin ime korišten, te možete slati i primati podatke o imenima, semantički - oni su kao kanali.

## Započnimo

Nema IDE za Rholang. S Rholangom započinjemo odabirom jedne od opcija u nastavku.
* __Pokrenite Rholang na RNode__ - Napišite Rholang ugovore u uredniku po svom izboru i pokrenite ih na RNode pomoću REPL ili EVAL načina. [Započnite](https://github.com/rchain/rchain/releases) s najnovijom verzijom RNode.
* __Pokrenite Rholang na web sučelju__ - Ovaj [web interface](http://rchain.cloud) stvorio je član RChain zajednice.
* __Napišite Rholang koristeći IntelliJ plugin__ - Ovaj [plugin Rholang IntelliJ](https://github.com/tgrospic/rholang-idea) stvorio je član RChain zajednice.

## Ugovori i slanje podataka

    1 new HelloWorld in {
    2   contract HelloWorld(return) = {
    3     return!("Hello, World!")
    4   } |
    5   new myChannel in {
    6     HelloWorld!(*myChannel)
    7   }
    8 }

1-2) Da biste stvorili novi privatni kanal, koristimo `new ... in` izgradnji. Nijedan drugi postupak ne može slati ili primati poruke putem ovog kanala osim ako eksplicitno pošaljemo ovaj kanal u drugi postupak. Ovaj proces počinje stvaranjem novog imena `HelloWorld` i zatim se na njemu izvodi ugovor. `contract` stvara proces koji stvara kopiju svog tijela kad god primi poruku. 

3) Na povratnom kanalu šaljemo proces koji je niz `"Hello, World!"`.

6) Mi šaljemo kanal `myChannel` na ugovor na `HelloWorld`. Operater `*` ne označava kanal kako bi dobio svoj temeljni proces. U Rholangu možete poslati samo procese preko kanala; kanale ne možete izravno slati preko kanala. Stoga koristimo `*` kako bi privatni kanal pretvorili u proces prije slanja.

## Primanje podataka

    1 new HelloAgain in {
    2   contract HelloAgain(_) = {
    3     new chan in {
    4       chan!("Hello again, world!") |
    5       for (@text <- chan) { Nil }
    6     }
    7   } | HelloAgain!(Nil)
    8 }


2) Ugovori uzimaju barem jedan parametar, ali ga možemo odbaciti vezanjem na zamjenski znak `_`.

3) Stvaramo novi kanal `chan`.

4) Šaljemo niz procesa `"Hello again, world!"` preko novog kanala.

5) Slušamo na novom kanalu kako bismo čuli barem jednu poruku. Operacija `for` blokira dok na kanalu `chan` ne bude prikazana poruka. U Rholangu možete na kanalima primati samo imena (names) (imajte na umu da se to razlikuje od slanja!). Vezivanje na lijevoj strani `<-` u `for` zapravo je uzorak imena. U ovom primjeru obrazac je `@text`, što znači da je naziv koji se prima ustvari citirani postupak i želimo vezati taj proces u slobodnu varijablu `text`. Operacija `for` je baš kao i ugovor, osim što čita samo jednu poruku koja potom postaje njegovo tijelo, umjesto da zaokružuje kopiju svog tijela za svaku poruku. U ovom slučaju odlučili smo da ništa ne radimo `for` tijelo tako što ćemo jednostavno zaustaviti `Nil` proces, ipak u principu bismo željeli nastaviti s daljnjom obradom `text` sadržanog u `chan`. 

7) Pokrećemo ugovor.

## Promjenjivo stanje

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

1) Mi kreiramo novi kanal MakeCell, a zatim ga koristimo na liniji 3 kao naziv internog ugovora. Nijedan se proces, osim koda unutar ovog leksičkog opsega, ne može opozovati.

3) U `MakeCell` ugovoru se razmatraju tri argumenta. Prvi argument je inicijalna vrijednost koja se čuva u ćeliji. Drugi i treći argument su kanali preko kojih  će ćelija primiti zahtjeve za dobivanje i postavljanje vrijednosti. Imajte na umu kako želimo da prvi argument bude proces, a drugi i treći da budu imenovanje („names“), ali names uvijek primamo preko kanala, tako da je potrebno napraviti obrazac prvog argumenta koji počinje s `@` kako bi se naznačilo da je names koje primimo kao prvi argument ustvari citiran proces i upravo taj proces želimo vezati za varijablu.

4) Kako bi sačuvali vrijednost, kreiramo novi kanal. Ovaj kanal će imati najviše jednu poruku koja sadržava trenutnu vrijednost ćelije. 

5) Prije ove linije na kanalu `valueStore` nema poruka. Nakon što pošaljemo početnu vrijednost, ona postaje jedina vrijednost na tom kanalu.

6) Postavili smo ugovor za slušanje `get` kanala. Svaki put kada se pošalje poruka na `get`, izvršava se body ugovora.

7) Blokiramo sve dok ne dobijemo jednu poruku `valueStore` kanala. Pošto postoji najviše jedna poruka koja je ikada čekala na `valueStore`, čitanje poruke se ponaša slično kao zaključavanje.

8) Ponovo šaljemo trenutnu vrednost na `valueStore`, dozvoljavajući da se druge poruke obrađuju (npr. oslobađanje zaključavanja), a trenutnu vrijednost vratimo klijentu na `ack` kanal.

11) Istovremeno s `get` ugovorom, vodimo ugovor koji se sluša na `set`.

12) Dok nema poruke u `valueStore`, blokirat ćemo ga, a zatim  ćemo ga pročitati. Odbacit ćemo poruku koju pročitamo.

13) Šaljemo novu vrijednost za skladištenje na `valueStore` i signaliziramo da je operacija završena. 

18-36) Upotrebni kôd pokazuje stvaranje ćelije, dodjeljivanje početne vrijednosti 123, dobivanje te vrijednosti, postavljanje vrijednosti na 456, a zatim dobijanje i te vrijednosti.  

Obratite pozornost na duboke slojeve povratnog poziva. Rholang je dizajniran da napravi istovremene izračune prirodnim da se izraze; i kao posljedicu toga, od zavisnosti podataka implicira da sekvencioniranje na drugim jezicima mora biti eksplicitno.

## Iteracija i podudaranje

U kôdu u nastavku prikazujemo primjer iteracije liste.

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

3) `match` Konstrukcija koja omogućuje podudaranje, omogućuje i razvrstavanje varijabli podudaranjem.

4) Popratni obrasci podržavaju podudaranje s ostatkom popisa. Ako se `list` podudara s uzorkom para glave/repa onda izvršavamo glavno tijelo petlje.

5) Stvaramo kanal za upravitelja stavki da nas obavijesti da je to učinjeno s trenutnom stavkom.

6) Pozivamo procesor na stavku (item) i na kanal koji ga potvrđuje.

7) Kada kanal potvrdi stavku, vraćamo iterator na rep (od nazad).

10) Ako je popis prazan, signalizirat ćemo da je obrada dovršena.

14) Pozivamo iteraciju.

15-18) Ovaj `contract` ugovor će biti pozvan za svaku stavku na popisu. Na 17. retku, kažemo iteratoru da smo gotovi s ovom stavkom.

19) Ovaj `for` sadrži kod koji se treba zatvoriti nakon dovršetka iteracije.

## Mape

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
    38           for(_ <- ret){
    39             set!(1, *ret) | for(_ <- ret) {
    40               get!(*ret) | for(@r <- ret) {
    41                 //r is equal to 1
    42                 Nil
    43               }
    44             }
    45           }
    46         }
    47       }
    48     }
    49   }
    50 }

2) Jedan dizajn obrasca, koji se koristi u gore navedenom MakeCellovom ugovoru, mora primiti kanal od pozivatelja za svaki pojedini dio funkcionalnosti koji osigurava proces. Objektno orijentirani programer može reći da MakeCell zahtjeva od pozivatelja da osigura kanal za svaku metodu. MakeCoatCheck koristi više objektno orijentiran pristup, kao što ćemo vidjeti.

3) Stvaramo `port` ulazni kanal za interakciju s provjerom čuvanja stvari (coat check) kao i `table` koji će se koristiti za  pohranjivanje/dohvaćanje vrijednosti u čuvanju (coat check).

4) Šaljemo `port` (završnu točku u komunikacijskom procesu) pozivatelju kako bi mogli komunicirati o čuvanju vrijednosti (coat check).

5, 11, 17) Definiramo različite metode koje se mogu pozvati slanjem poruke na `port`. To se postiže navođenjem uzoraka koji se uzajamno isključuju i koji se mogu poredati na `port`, tako da prvi element poruke sadrži ime metode, dok su sljedeći elementi argument(i) i povratni kanal. Pomoću `<=` strelice umjesto `<-` strelice znači da su `for` "replicirane". To im daje jednako ponašanje kao i u `contract` tj. proces koji sluša poruke na `port` nastaje nakon mriješćenja instance svog tijela.

8) Iskorištavamo mogućnost citiranja bilo kojeg postupka kako bismo imali ime da bismo stvorili jedinstveni naziv za svaku vrijednost koju treba pohraniti. Postupak `*ticket | *table`  se proizvodi od strane istovremene kompozicije procesa proizvedenih završetkom citiranja naziva `ticket` i `table`. Tada se postupak može citirati kako bi se stvorio jedinstveni naziv koji se zatim koristi za pohranjivanje vrijednosti tako da ga pošalje na ime (name).

## Gladni filozofi i zastoj

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

U problemu Filozofa koji večeraju radi se o dvojici filozofa koji dijele samo jedan set pribora za jelo. Filozof1 sjedi na istočnoj strani dok Filozof2 na zapadnoj. Svaki treba oboje, i nož i vilicu kako bi jeo. Svaki odbija odustati od pribora dok ne iskoristi oboje za uzimanje zalogaja. Ako oba filozofa posegnu za priborom s desne strane, obojica će gladovati: Filozof1 uzima nož, Filozof2 uzima vilicu i nijedan ih više ne ispušta.

Evo kako rješiti problem:

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

4, 9) Spajanje operatora, oznaka točka-zarez ;, objavljuje kako nastavak slijedi samo ako postoji poruka na svakom od kanala istovremeno, spriječavajući gore spomenuti zastoj.

## Sigurno dizajnirani obrasci

U ovom dijelu opisujemo nekoliko obrazaca ili uzoraka dizajniranja. Ovi uzorci su prilagođeni iz Marc Stieglerove knjige [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf).

### Faceti

U MakeCell ugovoru, klijent omogućuje dva kanala, jedan za dobivanje vrijednosti i jedan za postavljanje. Ako klijent zatim prosljeđuje samo `get` za preuzimanje u drugi proces, taj proces efektivno ima read-only pogled ćelije.  

Kanali kao`get` i `set` nazivaju se "faceti" procesa. Oni obuhvaćaju ovlasti za obavljanje akcije. Ako je set channel public ili javni kanal kao `@"Foo"`, tada svatko tko nauči ili čak pogodi string `"Foo"`ima ovlasti postaviti vrijednost ćelije. S druge strane, ako je  `set` kanal kreiran s `new` novim operatorom, tada ne postoji način za neki drugi proces da kreira `set` kanal; mora se delegirati procesu direktno kako bi ga  proces koristio.  

Primjetite kako su ovlasti od `get` i `set` također imaju ovlasti za presresti poruke poslane ćeliji:

    for (@ret <- get) { P } | 
    for (@ret <- get) { Q } | 
    get!(*ack)

Ovaj izraz posjeduje dva procesa slušanja na kanalu `get` i jednu poruku poslanu preko`get`. Samo jedan od dva procesa biti će sposobna primiti poruku.

Primanjem kanala od klijenta za getting i setting, MakeCellov ugovor ostavlja odluku o tome koliko su javni ti kanali klijentu. MakeCoatCheck ugovor s druge strane konstruira vlastite kanale i otkriva metode klijentu, stavljajući ga u položaj pojačanja jamstava o privatnosti.

### Prigušivanje prosjeditelja

U MakeCoatCheck samo je jedan kanal i poruke se interno šalju. Za dobivane istog učinka kao kod faceta samo za čitanje, možemo stvoriti proces proslijeđivanja koji jednostavno zanemari svaku poruku koju ne želi proslijediti. Ugovor u nastavku samo proslijeđuje metodu "get".

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
    
### Revocation ili opozivanje

Možemo implementirati revocation kreirajući proslijeđivač sa prekidačem za ubijanje.

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

3) 	Kreiramo port za slušanje poziva metode i kanal `forwardFlag` za pohranjivanje da li želite prosljeđivati poruke.

4) Vraćamo kanal na koji klijenti šalju zahtjeve i kanal na koji će poslati signal za ubijanje.

5) Postavili smo početno stanje `forwardFlag` na istinito.

6-11) Čitamo u proizvoljnoj poruci, dobijemo i zamijenimo vrijednost zastave. Ako je zastavica istina, proslijedit ćemo poruku cilju ili `target`.

12-14) Ako je poruka ikad poslana na `kill` kanalu, postavljamo `forwardFlag` na lažno. Postupak proslijeđivanja stopira proslijeđivanje poruka.

### Kompozicija

Kombinirajući prigušeni proslijeđivač s opozivajućim proslijeđivačem, dobivamo obje karakteristike:

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

### Loging proslijeđivač

Loging proslijeđivač može snimati sve poruke poslane kanalom i to ponovnim odzivom na drugi kanal.

    contract MakeLoggingForwarder(target, logger, ret) = {
      new port in {
        ret!(*port) |
        contract port(@msg) = {
          target!(msg) |
          logger!(msg)
        }
      }
    }

### Odgovornost

Pretpostavimo Alice ima kanal i htjela bi logirati Boba kako bi imao pristup. Bob bi htio delegirati korištenje kanala Carol i logirao njen pristup. Svaka strana je slobodna konstruirati vlastiti loging proslijeđivač oko kanala kojeg su primili. Alice će držati Boba odgovornim za bilo što Carol učini.

### Sealing i unsealing

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


Sealer/unsealer par daje istu funkcionalnost kao javni ključevi, ali bez kriptografije. To je samo prigušivanje kontrole sloja opisano iznad. Ovaj obrazac dizajna može se koristiti za potpisivanje nečega u ime korisnika. U Rholang blockchain tutorialu, vidjet ćemo da sealer/unsealer par čak radi i kao potpisivanje/ovjeravanje par ključeva na blockchainu jer nema tajni za pohranu, samo nepatvorena imena da budu nedostupna.

### Čuvajte se slanja prigušivača

Osnovno načelo koje treba imati na umu s procesima RChain-a je onaj koji je sličan tradicionalnijim web aplikacijama: bez obzira na kod koji šaljete drugoj strani može se rastaviti. Još od kasnih 1990-ih kada je kupnja stvari preko weba postala moguća, [postojale su platforme za e-trgovinu](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/) gdje se platforma oslanjala na korisničke preglednike da pošalje točnu cijenu artikla natrag njemu. Autori nisu razmišljali o korisniku koji otvara alate za razvojne programere i mijenja cijenu prije nego je poslano natrag. Pravi je način izgradnje platforme za e-trgovinu pohraniti cijene na poslužitelj i provjeriti ih tamo.

Pretpostavimo da je Bob spreman pokrenuti neki kod za Alice; ima ugovor koji kaže nešto poput, "Preuzmite postupak s ovog kanala i pokrenite ga."

    for (p <- x) { *p }

To je baš kao da je web preglednik spreman za pokretanje JavaScript koda koji dolazi s web mjesta. Ako Alice šalje Bobu prigušenog otpremnika, Bob može koristiti uzorke koji odgovaraju proizvodnji u Rholangu te razdvojiti proces i dobiti pristup temeljnom resursu. Umjesto, kao u primjeru e-trgovine, Alice treba samo poslati kod koji prosljeđuje zahtjeve vlastitim procesima i učiniti prigušenje tamo.

## Zaključak

RChain je jezik dizajniran za upotrebu na blockchainu, ali nismo spomenuli ništa o nodes, namespaces, wallets, Rev i phlogiston, strukturi mreže, ili Casperu. Predstojeći dokument će se odnositi na sva ova pitanja i još mnogo toga.

Nadamo se da će gore navedeni primjeri izazvati želju za pisanjem još više kodova i pokazati jednostavnost izražavanja istovremenih projekata.
