1. Let's hash some rholang program and print out it in base16. In rholang:
```
new x, y, stdout(`rho:io:stdout`) in { 
   x!(@"name"!("Joe") | @"age"!(40)) |  // (1)
   for (@r <- x) { @"keccak256Hash"!(r.toByteArray(), *y) } |  // hash the program from (1)
   for (@h <- y) { stdout!(h) }  // print out the keccak256 hash
}
```
This will print the hash of our program `(1)` : 
`a6da46a1dc7ed715d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e`

2. We need pair of keys, let's generate some with `Ed25519` available in the project. In scala console:
```
import coop.rchain.crypto.signatures._
import coop.rchain.crypto.codec._

val keyPair = Ed25519.newKeyPair
val secKey = Base16.encode(keyPair._1)
// secKey: String = f6664a95992958bbfeb7e6f50bbca2aa7bfd015aec79820caf362a3c874e9247
val pubKey = Base16.encode(keyPair._2)
// pubKey: String = 288755c48c3951f89c5f0ffe885088dc0970fd935bc12adfdd81f81bb63d6219
```

3. Now we need to sign the hash we obtained in first step. Because we print out the base16 representation of underlying byte arrays for hashes we need to decode those strings.
```
val signature = Ed25519.sign(Base16.decode("a6da46a1dc7ed715d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e"), Base16.decode(secKey))
val base16Repr = Base16.encode(signature)
// d0a909078ce8b8706a641b07a0d4fe2108064813ce42009f108f89c2a3f4864aa1a510d6dfccad3b62cd610db0bfe82bcecb08d813997fa7df14972f56017e0b
```

4. Now we can pass the signature and public key to our rholang program to verify it using crypto functions available. 

`ed25519Verify` channel expects four arguments as follows:
- data to verify. In our case this will be the keccak256 hash of our rholang program. The hash is represented in base16 so we need to call `hexToBytes` on it to turn the string into byte array
- signature. Again we have hexadecimal string so we need to turn it into byte array (`hexToBytes`)
- public key (same as for signature). 
- channel on which the result of verification will be returned

So, in rholang:
```
new x, stdout(`rho:io:stdout`) in { 
  @"ed25519Verify"!("a6da46a1dc7ed715d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e".hexToBytes(), "d0a909078ce8b8706a641b07a0d4fe2108064813ce42009f108f89c2a3f4864aa1a510d6dfccad3b62cd610db0bfe82bcecb08d813997fa7df14972f56017e0b".hexToBytes(),"288755c48c3951f89c5f0ffe885088dc0970fd935bc12adfdd81f81bb63d6219".hexToBytes(), *x) | 
  for (@v <- x) { stdout!(v) } 
} 

```
and we should see: 
```
@{true}
```

which means that our hash and signature match with public key.

If we for example pass in corrupted hash (the change is emphasized with ** - 71 changed to 61):
```
new x, stdout(`rho:io:stdout`) in { 
   @"ed25519Verify"!("a6da46a1dc7ed**61**5d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e".hexToBytes(), "d0a909078ce8b8706a641b07a0d4fe2108064813ce42009f108f89c2a3f4864aa1a510d6dfccad3b62cd610db0bfe82bcecb08d813997fa7df14972f56017e0b".hexToBytes(),"288755c48c3951f89c5f0ffe885088dc0970fd935bc12adfdd81f81bb63d6219".hexToBytes(), *x) | 
   for (@v <- x) { stdout!(v) } 
} 
```

we will get:
```
@{false}
```
