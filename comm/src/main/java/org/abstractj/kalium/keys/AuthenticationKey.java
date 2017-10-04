/**
 * Copyright 2015 Cisco Systems, Inc.
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

import org.abstractj.kalium.encoders.Encoder;

import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_AUTH_HMACSHA512256_BYTES;
import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_AUTH_HMACSHA512256_KEYBYTES;
import static org.abstractj.kalium.NaCl.sodium;
import static org.abstractj.kalium.crypto.Util.checkLength;
import static org.abstractj.kalium.crypto.Util.isValid;
import static org.abstractj.kalium.encoders.Encoder.HEX;


public class AuthenticationKey implements Key {

    private byte[] key;

    public AuthenticationKey(byte[] key) {
        this.key = key;
        checkLength(key, CRYPTO_AUTH_HMACSHA512256_KEYBYTES);
    }

    public AuthenticationKey(String key, Encoder encoder) {
        this(encoder.decode(key));
    }

    public byte[] sign(byte[] message) {
        byte[] mac = new byte[CRYPTO_AUTH_HMACSHA512256_BYTES];
        sodium().crypto_auth_hmacsha512256(mac, message, message.length, key);
        return mac;
    }

    public String sign(String message, Encoder encoder) {
        byte[] signature = sign(encoder.decode(message));
        return encoder.encode(signature);
    }

    public boolean verify(byte[] message, byte[] signature) {
        checkLength(signature, CRYPTO_AUTH_HMACSHA512256_BYTES);
        return isValid(sodium().crypto_auth_hmacsha512256_verify(signature, message, message.length, key), "signature was forged or corrupted");
    }

    public boolean verify(String message, String signature, Encoder encoder) {
        return verify(encoder.decode(message), encoder.decode(signature));
    }

    @Override
    public byte[] toBytes() {
        return key;
    }

    @Override
    public String toString() {
        return HEX.encode(key);
    }

}
