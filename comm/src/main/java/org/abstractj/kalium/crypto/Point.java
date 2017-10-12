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

import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_SCALARMULT_CURVE25519_SCALARBYTES;
import static org.abstractj.kalium.NaCl.sodium;
import static org.abstractj.kalium.crypto.Util.zeros;
import static org.abstractj.kalium.encoders.Encoder.HEX;

public class Point {

    private static final String STANDARD_GROUP_ELEMENT = "0900000000000000000000000000000000000000000000000000000000000000";

    private final byte[] point;

    public Point() {
        this.point = HEX.decode(STANDARD_GROUP_ELEMENT);
    }

    public Point(byte[] point) {
        this.point = point;
    }

    public Point(String point, Encoder encoder) {
        this(encoder.decode(point));
    }

    public Point mult(byte[] n) {
        byte[] result = zeros(CRYPTO_SCALARMULT_CURVE25519_SCALARBYTES);
        sodium().crypto_scalarmult_curve25519(result, n, point);
        return new Point(result);
    }

    public Point mult(String n, Encoder encoder) {
        return mult(encoder.decode(n));
    }

    @Override
    public String toString() {
        return HEX.encode(point);
    }

    public byte[] toBytes() {
        return point;
    }
}
