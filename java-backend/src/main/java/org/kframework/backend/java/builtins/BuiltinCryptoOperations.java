package org.kframework.backend.java.builtins;


import org.bouncycastle.jcajce.provider.digest.Keccak;
import org.bouncycastle.util.encoders.Hex;
import org.kframework.backend.java.kil.TermContext;

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
        Keccak.Digest256 keccakEngine = new Keccak.Digest256();
        byte[] digest = keccakEngine.digest(inputHexString.stringValue().getBytes());
        String digestString = Hex.toHexString(digest);
        return StringToken.of(digestString);

    }
}
