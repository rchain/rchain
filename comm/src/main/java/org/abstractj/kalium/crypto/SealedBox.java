package org.abstractj.kalium.crypto;

import org.abstractj.kalium.encoders.Encoder;

import static org.abstractj.kalium.NaCl.Sodium.CRYPTO_BOX_SEALBYTES;
import static org.abstractj.kalium.NaCl.sodium;
import static org.abstractj.kalium.crypto.Util.isValid;

public class SealedBox {

    private byte[] publicKey;
    private byte[] privateKey;

    public SealedBox(byte[] publicKey) {
        this.publicKey = publicKey;
        this.privateKey = null;
    }

    public SealedBox(String publicKey, Encoder encoder) {
        this(encoder.decode(publicKey));
    }

    public SealedBox(byte[] publicKey, byte[] privateKey) {
        this.publicKey = publicKey;
        this.privateKey = privateKey;
    }

    public SealedBox(String publicKey, String privateKey, Encoder encoder) {
        this(encoder.decode(publicKey), encoder.decode(privateKey));
    }

    public byte[] encrypt(byte[] message) {
        if (publicKey == null)
            throw new RuntimeException("Encryption failed. Public key not available.");
        byte[] ct = new byte[message.length + CRYPTO_BOX_SEALBYTES];
        isValid(sodium().crypto_box_seal(
                        ct, message, message.length, publicKey),
                "Encryption failed");
        return ct;
    }

    public byte[] decrypt(byte[] ciphertext) {
        if (privateKey == null)
            throw new RuntimeException("Decryption failed. Private key not available.");

        byte[] message = new byte[ciphertext.length - CRYPTO_BOX_SEALBYTES];
        isValid(sodium().crypto_box_seal_open(
                        message, ciphertext, ciphertext.length, publicKey, privateKey),
                "Decryption failed. Ciphertext failed verification");
        return message;
    }

}
