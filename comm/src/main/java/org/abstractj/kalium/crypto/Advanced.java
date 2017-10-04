/**
 * Copyright 2013 Bruno Oliveira, and individual contributors
 * <p/>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p/>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p/>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.abstractj.kalium.crypto;

import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_STREAM_KEYBYTES;
import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_STREAM_NONCEBYTES;
import static org.abstractj.kalium.NaCl.sodium;
import static org.abstractj.kalium.crypto.Util.checkLength;

public class Advanced {

    public byte[] crypto_stream_xsalsa20_xor(byte[] message, byte[] nonce, byte[] key) {

        checkLength(nonce, CRYPTO_STREAM_NONCEBYTES);
        checkLength(key, CRYPTO_STREAM_KEYBYTES);
        byte[] buffer = new byte[message.length];
        sodium().crypto_stream_xor(buffer, message, message.length, nonce, key);
        return buffer;

    }
}
