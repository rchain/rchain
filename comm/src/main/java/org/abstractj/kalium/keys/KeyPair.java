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

import org.abstractj.kalium.crypto.Point;
import org.abstractj.kalium.encoders.Encoder;

import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_BOX_CURVE25519XSALSA20POLY1305_PUBLICKEYBYTES;
import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SECRETKEYBYTES;
import static org.abstractj.kalium.NaCl.sodium;
import static org.abstractj.kalium.crypto.Util.checkLength;
import static org.abstractj.kalium.crypto.Util.zeros;

public class KeyPair {

    private byte[] publicKey;
    private final byte[] secretKey;

    public KeyPair() {
        this.secretKey = zeros(CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SECRETKEYBYTES);
        this.publicKey = zeros(CRYPTO_BOX_CURVE25519XSALSA20POLY1305_PUBLICKEYBYTES);
        sodium().crypto_box_curve25519xsalsa20poly1305_keypair(publicKey, secretKey);
    }

    public KeyPair(byte[] secretKey) {
        this.secretKey = secretKey;
        checkLength(this.secretKey, CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SECRETKEYBYTES);
        Point point = new Point();
        this.publicKey = point.mult(secretKey).toBytes();
    }

    public KeyPair(String secretKey, Encoder encoder) {
        this(encoder.decode(secretKey));
    }

    public PublicKey getPublicKey() {
        return new PublicKey(publicKey);
    }

    public PrivateKey getPrivateKey() {
        return new PrivateKey(secretKey);
    }
}
