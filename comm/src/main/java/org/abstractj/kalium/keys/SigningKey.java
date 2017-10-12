/**
 * Copyright 2013 Bruno Oliveira, and individual contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.abstractj.kalium.keys;

import jnr.ffi.byref.LongLongByReference;
import org.abstractj.kalium.crypto.Random;
import org.abstractj.kalium.crypto.Util;
import org.abstractj.kalium.encoders.Encoder;

import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_BOX_CURVE25519XSALSA20POLY1305_PUBLICKEYBYTES;
import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SECRETKEYBYTES;
import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_SIGN_ED25519_BYTES;
import static org.abstractj.kalium.NaCl.sodium;
import static org.abstractj.kalium.crypto.Util.checkLength;
import static org.abstractj.kalium.crypto.Util.isValid;
import static org.abstractj.kalium.crypto.Util.slice;
import static org.abstractj.kalium.crypto.Util.zeros;
import static org.abstractj.kalium.encoders.Encoder.HEX;

public class SigningKey {

    private final byte[] seed;
    private final byte[] secretKey;
    private final VerifyKey verifyKey;

    public SigningKey(byte[] seed) {
        checkLength(seed, CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SECRETKEYBYTES);
        this.seed = seed;
        this.secretKey = zeros(CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SECRETKEYBYTES * 2);
        byte[] publicKey = zeros(CRYPTO_BOX_CURVE25519XSALSA20POLY1305_PUBLICKEYBYTES);
        isValid(sodium().crypto_sign_ed25519_seed_keypair(publicKey, secretKey, seed),
                "Failed to generate a key pair");

        this.verifyKey = new VerifyKey(publicKey);
    }

    public SigningKey() {
        this(new Random().randomBytes(CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SECRETKEYBYTES));
    }

    public SigningKey(String seed, Encoder encoder) {
        this(encoder.decode(seed));
    }

    public VerifyKey getVerifyKey() {
        return this.verifyKey;
    }

    public byte[] sign(byte[] message) {
        byte[] signature = Util.prependZeros(CRYPTO_SIGN_ED25519_BYTES, message);
        LongLongByReference bufferLen = new LongLongByReference(0);
        sodium().crypto_sign_ed25519(signature, bufferLen, message, message.length, secretKey);
        signature = slice(signature, 0, CRYPTO_SIGN_ED25519_BYTES);
        return signature;
    }

    public String sign(String message, Encoder encoder) {
        byte[] signature = sign(encoder.decode(message));
        return encoder.encode(signature);
    }

    public byte[] toBytes() {
        return seed;
    }

    @Override
    public String toString() {
        return HEX.encode(seed);
    }
}
