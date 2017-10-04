package org.abstractj.kalium.crypto;

import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_PWHASH_SCRYPTSALSA208SHA256_OUTBYTES;
import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_PWHASH_SCRYPTSALSA208SHA256_STRBYTES;
import static org.abstractj.kalium.NaCl.sodium;
import org.abstractj.kalium.encoders.Encoder;

public class Password {

    public Password() {
    }
    public byte[] deriveKey(int length, byte[] passwd, byte[] salt, int opslimit, long memlimit) {
        byte[] buffer = new byte[length];
        sodium().crypto_pwhash_scryptsalsa208sha256(buffer, buffer.length, passwd, passwd.length, salt, opslimit, memlimit);
        return buffer;
    }

    public String hash(byte[] passwd, Encoder encoder, byte[] salt, int opslimit, long memlimit) {
        byte[] buffer = deriveKey(CRYPTO_PWHASH_SCRYPTSALSA208SHA256_OUTBYTES, passwd, salt, opslimit, memlimit);
        return encoder.encode(buffer);
    }

    public String hash(int length, byte[] passwd, Encoder encoder, byte[] salt, int opslimit, long memlimit) {
        byte[] buffer = deriveKey(length, passwd, salt, opslimit, memlimit);
        return encoder.encode(buffer);
    }

    public String hash(byte[] passwd, Encoder encoder, int opslimit, long memlimit) {
        byte[] buffer = new byte[CRYPTO_PWHASH_SCRYPTSALSA208SHA256_STRBYTES];
        sodium().crypto_pwhash_scryptsalsa208sha256_str(buffer, passwd, passwd.length, opslimit, memlimit);
        return encoder.encode(buffer);
    }

    public boolean verify(byte[] hashed_passwd, byte[] passwd) {
        int result = sodium().crypto_pwhash_scryptsalsa208sha256_str_verify(hashed_passwd, passwd, passwd.length);
        return result == 0;
    }
}
