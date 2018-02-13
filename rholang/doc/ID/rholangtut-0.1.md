# Tutorial Rholang

Rholang adalah bahasa pemrograman baru yang dirancang untuk digunakan dalam sistem terdistribusi. Seperti semua hal yang baru lahir, ia berkembang dan berubah dengan cepat; dokumen ini menjelaskan sintaks yang akan digunakan dalam rilis 0,1 SDK.

Rholang "berorientasi pada proses": semua perhitungan dilakukan dengan cara menyampaikan pesan. Pesan diteruskan pada "saluran", yang agak seperti antrian tapi agak seperti antrian. Rholang benar-benar asinkron, dalam arti bahwa saat Anda dapat membaca pesan dari saluran dan kemudian melakukan sesuatu dengannya, Anda setidaknya tidak dapat mengirim pesan, bukan tanpa secara eksplisit menunggu pesan pengakuan dari penerima.

## Contracts and sending data

    1 new helloWorld in {
    2   contract helloWorld(name) = {
    3     "Hello, ".display(name, "!\n")
    4   } |
    5   helloWorld("Joe")
    6 }

1) Program Rholang adalah proses tunggal. Proses ini dimulai dengan membuat saluran baru bernama `helloWorld`. Untuk membuat saluran pribadi baru, kami menggunakan konstruksi 'baru ... dalam'. Tidak ada proses lain yang dapat mengirim atau menerima pesan melalui saluran ini kecuali jika kami secara eksplisit mengirim saluran ini ke proses lainnya.

2) Produksi `kontrak` menciptakan proses yang menumbuhkan salinan tubuhnya setiap kali menerima pesan.

3) Metode `display` dari string menulis ke standar. Dibutuhkan daftar string untuk dicetak selanjutnya. Oleh karena itu, agar ini berhasil, pesan `nama` harus berupa string.

4) Kami mengirim string `" Joe "` melalui saluran `helloWorld`.

## Menerima data

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

2) Kontrak mengambil setidaknya satu parameter, tapi kita bisa membuangnya dengan mengikatnya ke variabel yang tidak pernah kita gunakan.

3) Kami membuat channel baru `chan`.

4) Kami mengirim proses string `" Halo lagi, dunia! "` Atas saluran baru.

5) Kami mendengarkan saluran baru untuk satu pesan. Blok operasi 'untuk` sampai ada pesan yang tersedia di saluran `chan`. Operasi `untuk` sama seperti kontrak kecuali hanya membaca satu pesan dan kemudian menjadi tubuhnya alih-alih mengotori salinan tubuhnya untuk setiap pesan.

## Keadaan tidak stabil

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

1) Kami membuat saluran baru MakeCell dan kemudian menggunakannya pada baris 3 sebagai nama kontrak internal. Tidak ada proses selain kode di dalam lingkup leksikal ini yang bisa memintanya.

3) Kontrak `MakeCell` mengambil tiga argumen. Argumen pertama adalah nilai awal yang akan disimpan di dalam sel. Argumen kedua dan ketiga adalah saluran dimana sel akan menerima permintaan untuk mendapatkan dan menetapkan nilainya.

4) Untuk menyimpan nilainya, kita membuat channel baru. Saluran ini akan memiliki paling banyak satu pesan di dalamnya yang berisi nilai sel saat ini.

5) Sebelum baris ini, tidak ada pesan pada saluran `valueStore`. Setelah kami mengirim nilai awal, itu adalah satu-satunya nilai pada saluran itu.

6) Kami membuat kontrak untuk mendengarkan saluran `get`. Setiap kali pesan dikirim pada `get`, badan kontrak akan dieksekusi

7) Kita blok sampai kita mendapatkan satu pesan dari channel `valueStore`. Karena paling banyak satu pesan yang pernah menunggu `valueStore`, membaca pesan berperilaku seperti mendapatkan kunci.

8) Kami mengirim nilai sekarang pada `valueStore` lagi, membiarkan pesan lain diproses, dan kami mengirimkan nilai saat ini kembali ke klien pada saluran` ack`.

11) Sejalan dengan kontrak `get`, kita menjalankan kontrak mendengarkan pada` set`.

12) Kita blok sampai ada pesan di `valueStore`, lalu membacanya. Kita membuang pesan yang kita baca.

13) Operasi `match` tidak merusak ikatan, memisahkan pasangan tupel` `ke dalam komponennya dan memberikan nama kepada mereka.

14) Kami mengirim nilai baru untuk disimpan di `valueStore` dan memberi sinyal bahwa operasi telah selesai.

21-36) Kode penggunaan menunjukkan pembuatan sel, menetapkan nilai awal 123, mendapatkan dan mencetak nilai tersebut, menetapkan nilainya menjadi 456, kemudian mendapatkan dan mencetak nilai tersebut.

Perhatikan lapisan balik callback yang dalam. Rholang dirancang untuk membuat perhitungan paralel yang alami untuk diungkapkan; Sebagai konsekuensinya, ketergantungan data yang tersirat dalam urutan dalam bahasa lain harus dibuat eksplisit.

## Iterasi dan pencocokan

Pada kode di bawah ini, `iterate` pertama-tama mengirim sebuah channel` next` over `iterator`, dan kemudian untuk setiap pesan yang diterima pada` next` kirim pasangan yang berisi item berikutnya dalam daftar dan apakah iterasi telah dilakukan.

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

7) Konstruksi `pertandingan .. dengan` memungkinkan penghancuran mengikat.

8) Metode `nth` pada tupel memungkinkan penggambaran elemen individual.

16) Tupel memiliki metode `size`.

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

2) Salah satu pola desain, yang digunakan dalam kontrak MakeCell di atas, adalah menerima dari pemanggil saluran untuk setiap bagian fungsi yang berbeda yang disediakan oleh sebuah proses. Pemrogram berorientasi obyek mungkin mengatakan bahwa MakeCell mewajibkan pemanggil untuk menyediakan saluran untuk setiap metode. Cocok dicoba sesuai urutan kode tersebut muncul; Jika tidak ada yang cocok, blok `match` akan dievaluasi ke proses` Nil`. MakeCoatCheck menggunakan pendekatan yang lebih berorientasi objek, seperti yang akan kita lihat.

3-4) Setiap cek mantel memiliki peta reentrant yang bisa diubah untuk menyimpan barang. Kami menyimpan peta yang baru dibangun di mapStore. Ini memiliki API berikut:

    insert(key, value)
    insertMany(key1, val1, key2, val2, ..., keyn, valn)
    getOrElse(key, default)
    get(key)

6) Kami mengharapkan empat argumen setiap saat; kita juga bisa mengharapkan sebuah tuple tunggal dan menggunakan destrukturisasi untuk pengiriman berdasarkan metode dan panjang tuple.

## Makan filsuf dan kebuntuan

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

Masalah filsuf makan memiliki dua filsuf yang hanya memiliki satu set perak. Filsuf1 duduk di sisi timur meja sementara Philosopher2 duduk di sebelah barat. Masing-masing membutuhkan pisau dan garpu untuk makan. Masing-masing menolak untuk menyerahkan peralatan sampai dia telah menggunakan keduanya untuk menggigit. Jika kedua filsuf itu meraih yang pertama untuk perkakas di sebelah kanan mereka, keduanya akan kelaparan: Filsuf1 mendapatkan pisau itu, Philosopher2 mendapat garpu, dan keduanya tidak membiarkannya pergi.

Inilah cara mengatasi masalah:

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

4, 9) Operator join, dilambangkan dengan titik koma `;`, menyatakan bahwa kelanjutan hanya akan dilanjutkan jika ada pesan yang tersedia di masing-masing saluran secara bersamaan, mencegah kebuntuan di atas.

## Pola desain yang aman

Pada bagian ini kami mendeskripsikan beberapa pola desain. Pola-pola ini diadaptasi dari Marc Stiegler's _ _ PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf).

### Aspek

Dalam kontrak MakeCell, klien menyediakan dua saluran, satu untuk mendapatkan nilai dan satu untuk menyetelnya. Jika klien kemudian hanya melewati saluran `get` ke proses lain, proses itu secara efektif memiliki tampilan baca-saja sel.

Saluran seperti `get` dan` set` disebut "faset" dari proses. Mereka merangkum otoritas untuk melakukan aksinya. Jika saluran `set` adalah saluran publik seperti` @ "Foo" `, maka siapa saja yang bisa belajar atau bahkan menebak string` "Foo" `memiliki wewenang untuk mengatur nilai sel. Di sisi lain, jika saluran `set` dibuat dengan operator` baru`, maka tidak ada jalan untuk proses lain untuk membangun saluran `set`; itu harus dilewatkan ke proses secara langsung agar proses menggunakannya.

Perhatikan bahwa jika `get` dan` set` tidak dibuat sebagai bagian dari iopairs, maka kepemilikan saluran tersebut juga berwenang untuk mencegat pesan yang dikirim ke sel:

    for (ret <- get) { P } | 
    for (ret <- get) { Q } | 
    get(ack)

Istilah ini memiliki dua proses mendengarkan pada saluran `get` dan satu pesan yang dikirim melalui` get`. Hanya satu dari dua proses yang bisa menerima pesan tersebut.

Dengan menerima saluran dari klien untuk mendapatkan dan menetapkan, kontrak MakeCell meninggalkan keputusan tentang bagaimana mengarahkan saluran tersebut ke klien. Kontrak MakeCellFactory, di sisi lain, membuat saluran sendiri dan mengembalikannya ke klien, sehingga berada dalam posisi untuk menegakkan jaminan privasi.

### Atenuating forwarders

Dalam kontrak MakeCellFactory, hanya ada satu saluran dan pesan dikirim secara internal. Untuk mendapatkan efek yang sama seperti aspek read-only, kita bisa membuat proses forwarder yang mengabaikan pesan yang tidak ingin diteruskan. Kontrak di bawah hanya meneruskan metode "dapatkan".

    contract MakeGetForwarder(target, ret) = {
        new port in {
            ret(port) |
            contract port(tuple) = {
                tuple.nth(0) match with "get" => target(tuple)
            }
        }
    }

### Pencabutan

Kita bisa menerapkan pencabutan dengan membuat forwarder dengan kill switch.

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

2) Kami membuat port untuk mendengarkan pemanggilan metode dan saluran `forwardFlag` untuk menyimpan apakah akan meneruskan pesan.

3) Kami mengembalikan saluran tempat klien mengirim permintaan dan saluran untuk mengirim sinyal pembunuhan.

4) Kami menetapkan keadaan awal 'forwardFlag` ke true.

5-10) Kita membaca secara acak tupel dari bagian pesan dan mendapatkan dan mengganti nilai bendera. Jika bendera benar, kita meneruskan tupel pesan ke `target`.

11-13) Jika pesan dikirim ke saluran `kill`, kami menetapkan` forwardFlag` ke false. Proses forwarder kemudian berhenti meneruskan pesan.

### Komposisi

Dengan menggabungkan forwarder yang melemahkan dengan forwarder yang dapat dicabut, kita mendapatkan kedua fitur tersebut:

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

### Logging forwarder

Forwarder logging dapat merekam semua pesan yang dikirim pada saluran dengan menggemakannya ke saluran kedua.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new port in {
            ret(port) |
            contract port(tuple) {
                target(tuple) |
                logger(tuple)
            }
        }
    }

### Akuntabilitas

Misalkan Alice memiliki saluran dan ingin mencatat akses Bob ke sana. Bob ingin mendelegasikan penggunaan saluran itu kepada Carol dan mencatat aksesnya. Masing-masing pihak bebas membangun penerangan mereka sendiri di sekitar saluran yang mereka terima. Alice akan menahan Bob untuk melakukan apa pun yang dilakukan Carol.

### Sealing dan unsealing

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


Pasangan sealer / unsealer memberi fungsi yang sama seperti kunci publik, namun tanpa kriptografi. Ini hanya pelemahan dari cek mantel yang dijelaskan di atas. Pola desain ini dapat digunakan untuk menandatangani sesuatu atas nama pengguna. Dalam tutorial blockchain Rholang, kita akan melihat bahwa itu bahkan bekerja pada blockchain karena tidak ada rahasia untuk disimpan, hanya nama yang tidak mungkin untuk dijaga agar tidak dapat diakses.

### Waspadalah terhadap pengiriman attenuator

Prinsip dasar yang perlu diingat dengan proses RChain adalah proses yang mirip dengan aplikasi web yang lebih tradisional: kode apa pun yang Anda kirim ke pihak lain dapat dibongkar. Sejak akhir 1990-an ketika membeli barang melalui web menjadi mungkin, [ada platform e-commerce](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce- kesalahan keamanan /) di mana platform mengandalkan browser pengguna untuk mengirimkan harga barang yang benar kembali ke dalamnya. Penulis tidak memikirkan pengguna membuka alat pengembang dan mengubah harganya sebelum dikirim kembali. Cara yang tepat untuk membangun platform e-commerce adalah dengan menyimpan harga di server dan memeriksanya di sana.

Misalkan Bob bersedia menjalankan beberapa kode untuk Alice; dia memiliki kontrak yang mengatakan sesuatu seperti, "Dapatkan proses dari saluran ini dan jalankan."

    for (p <- x) { *p }

Ini seperti browser web yang mau menjalankan kode JavaScript dari situs web. Jika Alice mengirim Bob sebuah forwarder yang attenuating, Bob dapat menggunakan pola produksi yang cocok di Rholang untuk memisahkan proses dan mendapatkan akses ke sumber yang mendasarinya. Sebagai gantinya, seperti dalam contoh e-commerce, Alice hanya boleh mengirim kode yang meneruskan permintaan ke prosesnya sendiri dan melakukan atenuasi di sana.

## Kesimpulan

RChain adalah bahasa yang dirancang untuk digunakan pada blockchain, namun kami belum menyebutkan apapun tentang node, ruang nama, dompet, Rev dan phlogiston, struktur jaringan, atau Casper. Dokumen yang akan terbit akan membahas semua masalah ini dan banyak lagi.

Kami berharap bahwa contoh di atas memicu keinginan untuk menulis lebih banyak kode dan menunjukkan kemudahan untuk mengungkapkan desain konkuren.
