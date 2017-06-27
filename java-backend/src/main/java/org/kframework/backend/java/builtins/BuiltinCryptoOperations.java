// Copyright (c) 2016 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;


import org.apache.commons.codec.DecoderException;
import org.bouncycastle.jcajce.provider.digest.Keccak;
import org.bouncycastle.util.encoders.Hex;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.utils.errorsystem.KEMException;

/**
 * Builtins for Cryptographic Operations
 */
public final class BuiltinCryptoOperations {

    /**
     * Finds the SHA3 digest of the input.
     *
     * @param inputHexString - The String is expected to be formed such that each character in the string
     *                       represents a Hex Value, and can be directly encoded into a byte.
     * @return Output String (256 characters) such that each character represents an encoded Hex Value.
     */
    public static StringToken keccak256(StringToken inputHexString, TermContext context) {
        try {
            byte[] bytes = org.apache.commons.codec.binary.Hex.decodeHex(inputHexString.stringValue().toCharArray());
            Keccak.Digest256 keccakEngine = new Keccak.Digest256();
            byte[] digest = keccakEngine.digest(bytes);
            String digestString = Hex.toHexString(digest);
            return StringToken.of(digestString);
        } catch (DecoderException d) {
            throw KEMException.criticalError(d.getMessage());
        }
    }
}
