## 'new'

### Syntax
- `new NameDeclarations in { Process[Names Declared] }`

where `NameDeclarations` are a nonempty, comma separated list with terms of the form
- `NameVariable`
- `NameVariable( Uri )`

Examples:
- `new x,y,z in { Process[x,y,z] }`

Nested is equivalent to listed, i.e.

 new x,y,z in { Process[x,y,z] }

is equivalent to

  new x in {
   new y in {
    new z in {
      Process[x,y,z]
     }
    }
   }

s the free variables in `Process` and substitutes them with unforgeable names (see unforgeable names section).

This is to make a private channel. Makes concurrent computation without any issues of shared memory possible.

Sending on a public channel is pretty much never useful. So, no sends/listens on a public (forgeable) channel are committed to the tuple space.
