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

package org.abstractj.kalium.crypto;

import org.abstractj.kalium.encoders.Encoder;

import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_GENERICHASH_BLAKE2B_BYTES;
import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_HASH_SHA256_BYTES;
import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_HASH_SHA512_BYTES;
import static org.abstractj.kalium.NaCl.sodium;

public class Hash {

    public byte[] sha256(byte[] message) {
        byte[] buffer = new byte[CRYPTO_HASH_SHA256_BYTES];
        sodium().crypto_hash_sha256(buffer, message, message.length);
        return buffer;
    }

    public byte[] sha512(byte[] message) {
        byte[] buffer = new byte[CRYPTO_HASH_SHA512_BYTES];
        sodium().crypto_hash_sha512(buffer, message, message.length);
        return buffer;
    }

    public String sha256(String message, Encoder encoder) {
        byte[] hash = sha256(message.getBytes());
        return encoder.encode(hash);
    }

    public String sha512(String message, Encoder encoder) {
        byte[] hash = sha512(message.getBytes());
        return encoder.encode(hash);
    }


    public byte[] blake2(byte[] message) throws UnsupportedOperationException {
        byte[] buffer = new byte[CRYPTO_GENERICHASH_BLAKE2B_BYTES];
        sodium().crypto_generichash_blake2b(buffer, CRYPTO_GENERICHASH_BLAKE2B_BYTES, message, message.length, null, 0);
        return buffer;
    }

    public String blake2(String message, Encoder encoder) throws UnsupportedOperationException {
        byte[] hash = blake2(message.getBytes());
        return encoder.encode(hash);
    }

    public byte[] blake2(byte[] message, byte[] key, byte[] salt, byte[] personal) throws UnsupportedOperationException {
        byte[] buffer = new byte[CRYPTO_GENERICHASH_BLAKE2B_BYTES];
        sodium().crypto_generichash_blake2b_salt_personal(buffer, CRYPTO_GENERICHASH_BLAKE2B_BYTES,
                                                          message, message.length,
                                                          key, key.length,
                                                          salt, personal);
        return buffer;
    }
}
