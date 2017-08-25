requires "domains.k"

module CRYPTO-SYNTAX
    imports STRING
    imports KRYPTO

    syntax Output ::= "keccakDigestTest" "(" String ")"     [function]
                  |   "sha3DigestTest"   "(" String ")"     [function]
                  |   "sha256DigestTest" "(" String ")"     [function]

    syntax Program ::= Output | Program Program             [left]
endmodule

module CRYPTO
    imports CRYPTO-SYNTAX

    configuration <k>$PGM:Program</k>

    rule keccakDigestTest(DATASTR) => Keccak256(DATASTR)
    rule sha3DigestTest(DATASTR)   => Sha3256(DATASTR)
    rule sha256DigestTest(DATASTR) => Sha256(DATASTR)
endmodule
