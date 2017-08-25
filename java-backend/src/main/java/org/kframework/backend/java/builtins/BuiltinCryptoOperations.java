// Copyright (c) 2016 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;


import org.apache.commons.codec.binary.StringUtils;
import org.bouncycastle.jcajce.provider.digest.Keccak;
import org.bouncycastle.jcajce.provider.digest.SHA256;
import org.bouncycastle.jcajce.provider.digest.SHA3;
import org.bouncycastle.util.encoders.Hex;
import org.kframework.backend.java.kil.TermContext;

import java.util.Arrays;
import java.security.SignatureException;

/**
 * Builtins for Cryptographic Operations
 */
public final class BuiltinCryptoOperations {

    /**
     * Finds the keccak256 digest of the input.
     *
     * @param inputHexString - The String is expected to be formed such that each character in the string
     *                       represents a Hex Value, and can be directly encoded into a byte.
     * @return Output String (256 characters) such that each character represents an encoded Hex Value.
     */
    public static StringToken keccak256(StringToken inputHexString, TermContext context) {
        byte[] bytes = StringUtils.getBytesIso8859_1(inputHexString.stringValue());
        Keccak.Digest256 keccakEngine = new Keccak.Digest256();
        byte[] digest = keccakEngine.digest(bytes);
        String digestString = Hex.toHexString(digest);
        return StringToken.of(digestString);
    }

    /**
     * Finds the SHA3 digest of the input.
     *
     * @param inputHexString - The String is expected to be formed such that each character in the string
     *                       represents a Hex Value, and can be directly encoded into a byte.
     * @return Output String (256 characters) such that each character represents an encoded Hex Value.
     */
    public static StringToken sha3256(StringToken inputHexString, TermContext context) {
        byte[] bytes = StringUtils.getBytesIso8859_1(inputHexString.stringValue());
        SHA3.Digest256 sha3engine = new SHA3.Digest256();
        byte[] digest = sha3engine.digest(bytes);
        String digestString = Hex.toHexString(digest);
        return StringToken.of(digestString);
    }

    /**
     * Finds the SHA256 digest of the input.
     *
     * @param inputHexString - The String is expected to be formed such that each character in the string
     *                       represents a Hex Value, and can be directly encoded into a byte.
     * @return Output String (256 characters) such that each character represents an encoded Hex Value.
     */
    public static StringToken sha256(StringToken inputHexString, TermContext context) {
        byte[] bytes = StringUtils.getBytesIso8859_1(inputHexString.stringValue());
        SHA256.Digest sha2engine = new SHA256.Digest();
        byte[] digest = sha2engine.digest(bytes);
        String digestString = Hex.toHexString(digest);
        return StringToken.of(digestString);
    }

    /**
     * Recovers the ECDSA Public key from a message hash and signature
     * @param messageHash a 32-character string in Latin-1 encoding representing the 32-byte message hash of the signed message
     * @param v The recovery id, in the range 27-34, to use to recover the correct public key
     * @param r The r component of the message signature, as a 32-character Latin-1 string
     * @param s The s component of the message signature, as a 32-character Latin-1 string
     * @return Output String (64 characters) in Latin-1 encoding representing the public key recovered
     * */
    public static StringToken ecdsaRecover(StringToken messageHash, IntToken v, StringToken r, StringToken s, TermContext context) {
        byte[] hashBytes = StringUtils.getBytesIso8859_1(messageHash.stringValue());
        byte vByte = v.bigIntegerValue().byteValueExact();
        byte[] rBytes = StringUtils.getBytesIso8859_1(r.stringValue());
        byte[] sBytes = StringUtils.getBytesIso8859_1(s.stringValue());
        try {
            ECDSARecover key = ECDSARecover.signatureToKey(hashBytes, rBytes, sBytes, vByte);
            return StringToken.of(Arrays.copyOfRange(key.getPubKey(), 1, 65));
        } catch (SignatureException e) {
            return StringToken.of("");
        }
    }

}
