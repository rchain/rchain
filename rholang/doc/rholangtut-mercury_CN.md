# Rholang教程

Rholang是一门设计在分布式系统中使用的新编程语言。与其他新生事物一样，它正在快速成长和演变；本文档描述了将在Mercury发行版中使用的语法。

Rholang 是"面向进程"的：所有的计算都通过消息传递完成。消息通过管道（channel）传递，它有点像消息队列，但是行为上更像集合而不是队列。Rholang是完全异步的，就意义而言，虽然你可以从一个管道上读取消息接着用它做些什么，但你不能做到发送一条消息然后一旦它被其他进程收到时本进程立刻做其他事情——至少不能不明确等待接收者发出的确认消息。

## 合约、反射和发送数据

    1 contract @"HelloWorld"(system) = {
    2     system!("print", "Hello, world!")
    3 }
1)  在互联网上，服务器有IP地址。域名服务（DNS）将字母数字组成的字符串映射为数字，就像电话簿一样。Rholang不使用数字或字符串，它是"反射性的"：所有管道都是通过一个序列化进程来命名的。所有进程的序列化都以`@`开头。这个合约监听管道上发送的消息，该管道由字符进程`"HelloWorld"`序列化来命名。为了尽量简洁，我们称之为："该合约监听了`@"HelloWorld"`命名的管道。"

合约声明了一个可以与其他进程进行交互的API。顶层合约都有相同的API：它们有一个参数，即`系统`进程管道。`系统`进程包含所有可能导致节点副作用的管道的名称。它是一个内置的进程，用于监听由方法名和若干参数组成的消息。

2) 感叹号操作符在左侧的通道上发送处于它(感叹号操作符)右侧消息。每条消息都是一个名称元组。如果我们发送了一个进程而不是名称，它会自动被序列化成一个名称。

在这种情况下，我们发送了一个包含两个进程的消息：字符串`"print"`和字符串`"Hello，world!"`。该`系统`进程是一个内置的进程，用于监听由方法名和一些参数组成的消息。在这种情况下，该进程将第二个参数回显到标准输出中。

## 新建管道、接收数据和模式

    1 contract @"HelloAgain"(system) = new chan in {
    2     chan!("Hello again, world!") |
    3     for (@text <- chan) system!("print", text)
    4 }

1) 创建一个新的个人管道，我们使用`new ... in`来构造。没有其他进程可以在这个管道发送或接收消息，除非我们明确地将此管道发送到其他进程。

2) 我们在新的管道发送字符串进程`"Hello again,world!"`。

3) 我们监听新管道上的消息。`for`操作将被阻塞，直到管道`chan`上有一条消息可用。

模式语言（也称为"空间类型"）包括进程语言和名称语言。每个具有自由变量的进程都是一个模式，该模式可以匹配具有相同结构的进程;自由变量绑定到该位置的子进程中。在第3行，我们期望一个单元素的元组。该元素是某些进程的序列化，因此以`@`开头。由于我们在第2行发送了一个字符串，自由变量`text`被绑定到该字符串进程。最后，第3行将该字符串转发给`系统`进程来打印。

接下来，我们将会探索更多模式的特性。

## 可修改状态、复制以及选择

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

1) 我们新建了一个名为MakeCell的管道，并在第三行中将它作为内部合约的名称。只有`@CellDemo`合约内部的代码才能够调用这个管道变量。

2) `MakeCell`合约有三个参数。第一个参数是一个进程变量, 初始值存储在该单元里。第二、三个参数是两个管道类型的变量，通过这两个管道可以完成对存储单元的数据访问以及数据读取。熟悉C++编程的开发者可以将管道变量粗略地类比成指针，在某种程度上，指针变量是一个指向一个地址的可序列化的数据类型。示例代码中操作符`@`可以类比成C++中的引用操作符`&`，表示引用传递。变量`init`绑定到进程而不是一个管道。在C++中，通过`*`操作符来可以将指针转换为引用；同样的，在Rholang中也是通过`*`操作符将管道类型的变量转换成进程。由于在Rholang中，我们只能通过管道来传递进程，因此`*`操作被频繁地使用。具体使用情况可见示例代码的奇数行。

为了存储变量，我们创建一个新的管道。该管道将拥有至多一条消息，包含存储单元目前的值。

4) 在这行之前，`valueStore`管道中没有任何消息。在我们向这个管道发送初始变量后，该值将是管道中的唯一值。

5) 与4并行, 我们尝试从`valueStore`中读取数据。双向管道意味着一旦我们在那个管道上获取了一条消息，我们应该在`for`后面立刻spawn一个进程副本并且立刻开始尝试再次从该管道中读值。

一旦管道有了消息，我们将该消息绑定到进程中的`value`变量中。

6-8) 第6行中`select`关键词意味着第七行和第八行中中的分支只有一个能够被执行。在该时刻，`valueStore`管道上没有等待(处理)的消息。

如果在管道`get`上有了消息，那么第7行的分支将被运行。变量ack被绑定到该消息，与此同时, 其他两件事也并行发生了: 我们读取得到的值将再次被发送到`valueStore`，并也发送到管道`ack`中。

消息是名称的元组。目前为止我们见过的所有消息都是单元组，但是第8行，我们正在等待一个双元组的消息。模式的第一部分将变量`newValue`绑定到消息的第一部分，同时模式的第二部分将变量`ack`管道绑定到一个管道。与第7行中我们通过`valueStore`发送`value`变量不同，我们在该行中发送的是`newValue`。

在第8行中，我们发送了一条零元组消息。在示例代码的第20行以及第22行中，我们使用了下划线表示我们将接收到空消息并直接丢弃。

9) 在这里，在`valueStore`管道上恰好又有一条消息。

13-31) 该示例代码演示了如何创建一个存储单元，在该存储单元中存储初始变量123，获取到该变量的值并且打印，将456存储到存储单元，然后继续获取到该变量且进行打印。

注意到在示例代码中有很深层次的回调函数。Rholang被设计成使并行计算能被自然表达。因此，在其他语言中隐式的串行数据依赖必须显式指定。

## 迭代

    1 contract @"IterateDemo"(system) = new chan in {
    2     [1,2,3].iterate(chan) |
    3     for (@num, ack << chan) system!("print", num, *ack)
    4 }

2) 方括号表示一个列表。列表是可变的，而用圆括号表示的元组则是不可变的。一些进程，比如Java对象支持的进程，用于有方法的; 在这，`iterate`方法正在被调用，并在一个channel来进行迭代。

3)  运算符`<<`表示"连续发送"，换句话说，在发送新消息之前，来自管道的消息需要确认。

系统进程的`print`方法接受一个或两个参数。在两个参数的情况下，第二个参数是打印完成后将发送确认消息的通道。`iterate`方法将接收到确认并发送下一条消息。一旦列表完成迭代，上述代码的第3行将会被执行为`Nil`进程，不做任何事情，并被垃圾回收。

## 模式匹配和剩余参数

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

2) 上面的 MakeCell 合约使用的一种设计模式，是从调用者接收一个管道，对应于进程提供的每个不同功能片。一个面向对象的程序员可能会说，MakeCell 要求调用者为每个方法提供一个通道。而Matches语法会按照它们出现在代码中的顺序尝试匹配; 如果都不匹配，则`match`块求值为`Nil`进程。

3) MakeCoatCheck 使用一个更加面向对象的方式。`(in, out):iopair`的结构可以创建一对耦合的管道。需要注意，一种典型的错误是：通过`in`管道发送消息或通过`out`管道接收消息。但是，任何通过`out`通道发送的消息都可以通过`in`通道接收。这允许我们可以返回一个`out`管道，在该管道上"方法调用"可以被实现并且不用要求其他进程拦截对我们进程的请求。

新的管道`table`将会被用于创建内部使用的管道。

5) 我们不断接受参数个数多于两个的消息。变量`method`被绑定到第一个参数；我们期望使用一个字符串来命名该方法，就像系统进程一样。变量`ack`被绑定到一个管道上，我们将在这个管道上发送任何方法调用的结果。变量`rest`绑定到消息体剩余部分。

6) `match ... case`的语法结构使得我们可以在进程结构上进行模式匹配。我们可以利用这点来进行消息分发。

7-12) 如果`method`是字符串`new`，接着我们假设在第8行中`rest`将是一个拥有一个元素的元组，即初始值。 我们创建一个`ticket`管道并使用`ack`管道返回它。 我们还将 `*ticket` 进程与 `*table`结合为一个新的进程中，然后从它们派生出一个管道名称。 因为只有我们可以访问`table`，所以只有我们可以操作由这种方式构建的通道上存储的数据。管道`ticket`变现为一张格子毛衣的可赎回的票据，并且管道`@(*ticket | *table)`是由那个键指向的表项。

13-19) 如果`method`是字符串`get`，那么我们在第14行假设`rest`将是只有一个元素的元组，这个元素就是我们要获取的项的ticket。`<!`操作符会从表中读取一个值并立即将其放回。

    for (y <! x) { P }

是下面这种语法的简化语法糖：

    for (y <- x) { x!(*y) | P }.

20-27) 如果`method`是字符串`"set"`，那么我们假设第21行中的`rest`将是一个包含两个元素的元组：key和new值。第22行丢弃该票的当前值，第23行发送新值，第24行发送已完成的信号。

## 错误处理

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

1-3) 我们可以指定多个管道，这些管道上的数据可以传回客户端。

4) 在这种情况下，`select`的行为很像在其他语言中的`try`。第5行和第10行的接收者中只有一个会进行; 他们会竞争接收信息。如果第1-3行有一个不变的结果，或是在`ret`上产生结果，或是在`err`上发生错误，那么就不会有竞争。 另一方面，如果我们想发送一个结果和一个错误，我们应该使用`for`代替：


    for (@info, ret, err <- channel) {
        // Either return a result on ret or an error on err
    } |
    for(result <- ret) {
        // Process result
    } |
    for(@"TypeError", msg <- err) {
        // Handle type error
    }

10) 这里的模式比我们之前见过的更复杂。 在这里，我们指定我们只需要两个名字的信息，第一个名字应该是序列化的字符串`TypeError`。 如果是别的，这个分支将不会继续。

## 哲学家就餐和死锁

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

哲学家就餐用说的是，两个哲学家共享只有一套银器。哲学家1坐在桌子的东边，而哲学家2坐在西边。每个人都需要一把刀和一把叉才能吃饭。每个人都拒绝松开器具，直到拥有2个器具从而吃到饭。 如果两位哲学家都先拿起他们右侧的器具，他们都将会饿死：哲学家1拿刀，哲人2拿叉子，都不放手。

下面是解决问题的方法：

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

4, 7) 用`;`表示连接运算符，声明只有在每个管道上同时存在可用的消息时才继续进行连续操作，从而防止上述死锁情况。

## 内置类型，永久发送，逻辑连接词和过滤

我们已经看到，那些我们可以在`match`结构或`for`结构中使用的模式都包含具有自由变量的进程。我们也可以使用描述内建进程的模式。 `Integer`模式描述所有32位有符号整型; 对于于`Double`，`String`和`Boolean`类型也是相似的情况。

我们可以使用逻辑连接词AND，OR和NOT来组合使用，分别用`&&`，`||`和`〜`表示。

    for (@(x && Integer) <- y) { P }

这个过程将一个过程变量`x`绑定到`y`上接收到的消息，但是它依然保持`x`是一个整型。

有一个美国人的谜语说："如果一枚硬币不是镍币而另一枚不是一角银币，你怎样才能得到15美分呢？" 镍币价值五美分，一角钱价值十美分。 假设我们在`coin`上发送了以下消息，编码价值低于15美分的各种美国硬币：

    coins!!(1) | coin!!(5) | coin!!(10)

`!!` 运算符代表着这些消息应该永久保留在通道上，而不是在被`for`结构接收时消耗掉。

    new x in { x!!("Hi there!") | for (msg <= x) { system!("print", msg) } }

只要在虚拟机上运行，上面的过程将打印出"Hi there!"

我们可以把之前的谜语编码为：

    for (@(x && ~5) <- coins; @(y && ~10) <- coins if x+y == 15) {
        system!("print", (x, y))
    }

第一种模式`@(x && ~5)`将匹配任何不是5的`coins`上的信息，并将`x`绑定到它; 类似地，第二个模式`@(y &&〜10)`将匹配任何不是10的`coins`上的信息，并将`y`绑定到它。`for`结构中的`if`语句只允许在其右侧的公式为`true`的情况下，继续进行; 在这种情况下，除非值总和为15，否则不会输出任何内容。

谜题的答案是"一个一角银币和一个镍币"，因为一角银币不是镍，镍币不是一角银币。 变量`x`将绑定到10，变量`y`将绑定到5，并且将输出`(10, 5)`。

## 安全的设计模式

在本节中，我们将介绍几种设计模式。这些模式改编自Marc Stiegler提出的[_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf) 。

### Facets

在MakeCell合约中，客户端提供了两个通道，一个用于读数据(get)，另一个用于写数据(set)。如果客户端只将`get`管道传递给另一个进程，那该进程实际上只有存储单元的一个只读视图。

像`get`和`set`管道被称为进程的"facets"。它们封装了行为的权限。如果`set`通道是一个公共的管道(如`@"Foo"`)，那么任何可以了解甚至猜出字符串`"Foo"`的人都有权限设置cell的值。另一方面，如果`set`管道是使用`new`操作符创建的话，那么其他进程没有任何办法来构建这个`set`通道；为了让一个进程能够使用它，那它就必须被直接地传给该进程。

注意，`get`和`set`并不是创建为一对iopairs。这些管道的所有者同样有权限拦截发送至cell的信息：

    for (ret <- get) { P } |
    for (ret <- get) { Q } |
    get!(*ack)

上面的代码有两个进程监听`get`通道，同时一个单独的消息通过`get`发送。两个进程中只有一个能够接收到这个消息。

通过接收来自客户端用于读(get)和写(set)的管道，MakeCell合约将这些管道的公开程度的决策权交给客户端。另一方面，MakeCellFactory合约构建了它自己的通道，并且将它们返回给客户端，因此它有能力确保私密性。

### 衰减转发器

在MakeCellFactory合约中，只有一个管道，并在内部分派消息。为了获得与只读facet相同的效果，我们可以创建一个转发器进程，它可以忽略任何它不想转发的消息。下面的合约只转发"get"方法。

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

### 撤销

我们可以通过创建一个带有kill开关的转发器来实现撤销。

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

2) 我们创建一个为方法调度的iopair和一个`forwardFlag`管道，来存储是否转发消息。

3) 我们返回客户端发送请求的管道和发送kill信号的管道。

4) 我们将`forwardFlag`的初始状态设置为true。

5-13) 我们读取一个消息部分的可变元组并获取flag的值。如果标志为真，我们将消息元组转发给`target`。

14-15) 如果在`kill`管道上发送了一条消息，我们将`forwardFlag`设置为false，停止转发消息。

### 组合

我们可以通过将一个弱化转发器与一个可撤回的转发器相结合，使其同时拥有两种特性：

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

### 日志转发器

日志转发器可以通过将通道里的消息反射到第二个通道来记录通道上发送的所有消息。

    contract MakeLoggingForwarder(target, logger, ret) = {
        new (portIn, portOut):iopair in {
            ret!(*portOut) |
            for (...@rest <= portIn) {
                target!(...rest) |
                logger!(...rest)
            }
        }
    }

### 问责制

假设Alice有一个通道，并想记录Bob的访问历史。而Bob想要将该通道的使用权委托给Carol并记录她的访问。每一方都可以任意地在收到的通道上建立自己的日志转发器。但是Alice会让Bob对Carol所做的任何事情负责。

### 封装和解封

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

一个封装器/解封器对提供了与公钥相同的功能，但是没有加密。这只是上述coat check的衰减器，这种设计模式可以用来代表用户签名。在Rholang区块链教程中，我们将看到它甚至可以在区块链上运行，因为没有任何秘密可以存储，只有不可伪造的名字不能被访问。

### 慎重发送衰减器

请记住RChain进程的一个类似于更传统Web应用程序的基本原则：你发送给另一方的任何代码都可以被反汇编。自从二十世纪九十年代后期，[网络购物成为现实以来](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/)，电子商务平台依赖用户浏览器将正确的商品价格返回给自己。而平台设计者并没有考虑到用户可能会打开开发者工具并在价格返回之前对其进行修改。构建电子商务平台的正确方式应当是将价格存储在服务器端并在本地检查。

假设Bob愿意为Alice运行一些代码，他有一个合约规定："从这个通道里获得一个进程，并运行它"。

    for (@P <- x) { P }

这就像一个网页浏览器愿意运行从网站获得的JavaScript代码。如果Alice发送一个弱化的转发器给Bob，那么Bob可以使用Rholang中的模式匹配功能，对该进程进行分解并访问底层资源。然而，就像在电子商务的例子中一样，Alice应该只发送代码，该代码将请求转发给她自己的进程，并在那里进行衰减。

## Conclusion

RChain是为了在区块链上使用而设计的一种语言，但是我们在本文中还没有提到关于节点，命名空间，钱包，REV（RChain中的基础货币）和phlogiston（类似于以太坊上的gas），网络架构或Casper的任何内容。在即将出版的文档里会对上述概念进行阐述。

我们希望前面的例子能够激发读者编程的愿望，同时也演示了表达并发设计并非难事。