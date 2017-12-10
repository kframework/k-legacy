// Copyright (c) 2013-2016 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;

import java.math.RoundingMode;

import org.kframework.backend.java.kil.*;
import org.kframework.mpfr.BigFloat;
import org.kframework.mpfr.BinaryMathContext;
import java.math.BigInteger;

/**
 * Table of {@code public static} methods on builtin floats.
 *
 * @author: dwightguth
 */
public class BuiltinFloatOperations {

    /**
     * Get the {@link BinaryMathContext} object to use to compute the arithmetic operation.
     *
     * Currently only floats with the same precision and exponent range can be used in a calculation.
     * Users will have to cast floating point types manually using round() if they wish.
     */
    private static BinaryMathContext getMathContext(FloatToken term1, FloatToken term2) {
        getExponent(term1, term2);
        if (term1.bigFloatValue().precision() != term2.bigFloatValue().precision()) {
            throw new IllegalArgumentException("mismatch precision: "
                    + "first argument precision is represented on " + term1.bigFloatValue().precision() + " bits "
                    + "while second argument precision is represented on " + term2.bigFloatValue().precision() + "bits");
        }
        return getMathContext(term1);
    }

    /**
     * Get the {@link BinaryMathContext} object to use to compute the arithmetic operation. Uses
     * {@link RoundingMode#HALF_EVEN} and the precision and exponent range of the {@link FloatToken}.
     */
    private static BinaryMathContext getMathContext(FloatToken term) {
        return new BinaryMathContext(term.bigFloatValue().precision(), term.exponent());
    }

    /**
     * Get the number of bits of exponent to use to compute the arithmetic operation.
     *
     * Currently only floats with the same precision and exponent range can be used in a calculation.
     * Users will have to cast floating point types manually using round() if they wish.
     */
    private static int getExponent(FloatToken term1, FloatToken term2) {
        if (term1.exponent() != term2.exponent()) {
            throw new IllegalArgumentException("mismatch exponent: "
                + "first argument exponent is represented on " + term1.exponent() + " bits "
                + "while second argument exponent is represented on " + term2.exponent() + "bits");
        }
        return term1.exponent();
    }

    public static IntToken precision(FloatToken term, TermContext context) {
        return IntToken.of(term.bigFloatValue().precision());
    }

    public static IntToken exponent(FloatToken term, TermContext context) {
        BinaryMathContext mc = getMathContext(term);
        return IntToken.of(term.bigFloatValue().exponent(mc.minExponent, mc.maxExponent));
    }

    public static IntToken exponentBits(FloatToken term, TermContext context) {
        return IntToken.of(term.exponent());
    }

    public static BoolToken sign(FloatToken term, TermContext context) {
        return BoolToken.of(term.bigFloatValue().sign());
    }

    public static BitVector<?> significand(FloatToken term, TermContext context) {
        BinaryMathContext mc = getMathContext(term);
        return BitVector.of(term.bigFloatValue().significand(mc.minExponent, mc.maxExponent), mc.precision);
    }

    public static FloatToken add(FloatToken term1, FloatToken term2, TermContext context) {
        return FloatToken.of(term1.bigFloatValue().add(term2.bigFloatValue(),
                getMathContext(term1, term2)), getExponent(term1, term2));
    }

     public static FloatToken sub(FloatToken term1, FloatToken term2, TermContext context) {
         return FloatToken.of(term1.bigFloatValue().subtract(term2.bigFloatValue(),
                 getMathContext(term1, term2)), getExponent(term1, term2));
    }

    public static FloatToken mul(FloatToken term1, FloatToken term2, TermContext context) {
         return FloatToken.of(term1.bigFloatValue().multiply(term2.bigFloatValue(),
                 getMathContext(term1, term2)), getExponent(term1, term2));
    }

    public static FloatToken div(FloatToken term1, FloatToken term2, TermContext context) {
        return FloatToken.of(term1.bigFloatValue().divide(term2.bigFloatValue(),
                getMathContext(term1, term2)), getExponent(term1, term2));
    }

    public static FloatToken rem(FloatToken term1, FloatToken term2, TermContext context) {
        return FloatToken.of(term1.bigFloatValue().remainder(term2.bigFloatValue(),
                getMathContext(term1, term2)), getExponent(term1, term2));
    }

    public static FloatToken pow(FloatToken term1, FloatToken term2, TermContext context) {
        return FloatToken.of(term1.bigFloatValue().pow(term2.bigFloatValue(),
                getMathContext(term1, term2)), getExponent(term1, term2));
    }

    public static FloatToken root(FloatToken term1, IntToken term2, TermContext context) {
        return FloatToken.of(term1.bigFloatValue().root(term2.intValue(),
                getMathContext(term1)), term1.exponent());
    }

    public static FloatToken unaryMinus(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().negate(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken abs(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().abs(
                getMathContext(term)), term.exponent());
    }

    /**
     * Rounds {@code term} to the specfiied precision and exponent range.
     *
     * Method is undefined if either integer is less than 2 because 2 is the minimum precision and exponent.
     * Two exponents must be used to store zero/subnormal/infinity/nan, so 4 is the minimum number of distinct
     * exponents a float can have. MPFR does not support floats with 1 bit of precision.
     * bits of the float.
     */
    public static FloatToken round(FloatToken term, IntToken precision, IntToken exponent, TermContext context) {
        if (precision.intValue() < 2 || exponent.intValue() < 2) {
            return null;
        }
        return FloatToken.of(term.bigFloatValue().round(
                new BinaryMathContext(precision.intValue(), exponent.intValue())),
                exponent.intValue());
    }

    public static FloatToken exp(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().exp(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken log(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().log(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken sin(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().sin(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken cos(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().cos(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken tan(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().tan(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken asin(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().asin(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken acos(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().acos(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken atan(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().atan(
                getMathContext(term)), term.exponent());
    }

    public static FloatToken atan2(FloatToken term1, FloatToken term2, TermContext context) {
        return FloatToken.of(BigFloat.atan2(term1.bigFloatValue(), term2.bigFloatValue(),
                getMathContext(term1, term2)), getExponent(term1, term2));
    }

    public static FloatToken max(FloatToken term1, FloatToken term2, TermContext context) {
        return FloatToken.of(BigFloat.max(term1.bigFloatValue(), term2.bigFloatValue(),
                getMathContext(term1, term2)), getExponent(term1, term2));
    }

    public static FloatToken min(FloatToken term1, FloatToken term2, TermContext context) {
        return FloatToken.of(BigFloat.min(term1.bigFloatValue(), term2.bigFloatValue(),
                getMathContext(term1, term2)), getExponent(term1, term2));
    }

    /**
     * Floating point equality. Uses {@link BigFloat#equalTo(BigFloat)} and not {@link BigFloat#equals(Object)}
     * in order to preserve the behavior that -0.0 ==Float 0.0 and NaN =/=Float NaN. ==K can be used to compare
     * identity on floating point numbers.
     */
    public static BoolToken eq(FloatToken term1, FloatToken term2, TermContext context) {
        return BoolToken.of(term1.bigFloatValue().equalTo(term2.bigFloatValue()));
    }

    public static BoolToken gt(FloatToken term1, FloatToken term2, TermContext context) {
        return BoolToken.of(term1.bigFloatValue().greaterThan(term2.bigFloatValue()));
    }

    public static BoolToken ge(FloatToken term1, FloatToken term2, TermContext context) {
        return BoolToken.of(term1.bigFloatValue().greaterThanOrEqualTo(term2.bigFloatValue()));
    }

    public static BoolToken lt(FloatToken term1, FloatToken term2, TermContext context) {
        return BoolToken.of(term1.bigFloatValue().lessThan(term2.bigFloatValue()));
    }

    public static BoolToken le(FloatToken term1, FloatToken term2, TermContext context) {
        return BoolToken.of(term1.bigFloatValue().lessThanOrEqualTo(term2.bigFloatValue()));
    }

    public static FloatToken int2float(IntToken term, IntToken precision, IntToken exponent, TermContext context) {
        return FloatToken.of(new BigFloat(term.bigIntegerValue(),
                new BinaryMathContext(precision.intValue(), exponent.intValue())), exponent.intValue());
    }

    /**
     * Converts {@code term} to an integer by truncating it. Function is only
     * defined on ordinary numbers (i.e. not NaN or infinity).
     */
    public static IntToken float2int(FloatToken term, TermContext context) {
        return IntToken.of(term.bigFloatValue().rint(getMathContext(term)
                .withRoundingMode(RoundingMode.DOWN)).toBigIntegerExact());
    }

    /**
     * mint2float converts a Bitvector or MInt {@code term} to an single or double precision float point value.
     */
    public static FloatToken mint2float(BitVector term, IntToken precision, IntToken exponentBits, TermContext context) {


        int termBitwidth = term.bitwidth();
        int expectedPrecision = precision.intValue();
        int expectedExponentBits = exponentBits.intValue();

        // Sanity Checks.
        if(termBitwidth != expectedExponentBits + expectedPrecision) {
            throw new IllegalArgumentException("A float with requested precision and exponentBit cannot be obtained from the input MInt");
        }

        if(termBitwidth != 32 && termBitwidth != 64) {
            throw new IllegalArgumentException("Illegal bitwidth provided: "
                    + "Only 32 or 64 are supported in order to obtain a single precision or double precision floating point value");
        }

        // Determine the sign.
        boolean sign = !term.extract(0,1).isZero();

        // Determine if a double or single precision floating point is requested and fix constants
        // based on them.
        boolean isDoublePrecision = (precision.intValue() == 53 && exponentBits.intValue() == 11);
        int beginExponent = 1, endExponent = 0 , beginSignificand = 0 , endSignificand = 0;
        if(isDoublePrecision) {
            endExponent = 12;
            beginSignificand = 12;
            endSignificand = 64;
        } else {
            endExponent = 9;
            beginSignificand = 9;
            endSignificand = 32;
        }

        BitVector biasedExponentBV = term.extract(beginExponent, endExponent);
        BitVector significandBV = term.extract(beginSignificand, endSignificand);

        // Case I: biasedExponentBV == 0H and significand == 0H, return +0 or -0.
        if(biasedExponentBV.isZero() && significandBV.isZero()) {
            if(sign) {
                return FloatToken.of(BigFloat.negativeZero(precision.intValue()), exponentBits.intValue());
            } else {
                return FloatToken.of(BigFloat.zero(precision.intValue()), exponentBits.intValue());
            }
        }

        // Case II: biasedExponentBV == all Ones and significandBV == 0H, return +inf/-inf
        //          biasedExponentBV == all Ones and significandBV != 0H, return nan.
        if(biasedExponentBV.eq(BitVector.of(-1, expectedExponentBits)).booleanValue()) {
            if(significandBV.isZero()) {
                if(sign) {
                    return FloatToken.of(BigFloat.negativeInfinity(precision.intValue()), exponentBits.intValue());
                } else {
                    return FloatToken.of(BigFloat.positiveInfinity(precision.intValue()), exponentBits.intValue());
                }
            } else {
                return FloatToken.of(BigFloat.NaN(precision.intValue()), exponentBits.intValue());
            }
        }

        // Case III: Sub-Nornals: biasedExponentBV == 0H and significand != 0H
        //               Normals: biasedExponentBV > 0H and biasedExponentBV < all Ones
        // For sub-normals, 0 need to be prepended to significandBV.
        // For Normals, 1 need to be appended.
        if(biasedExponentBV.isZero()) {
            significandBV = BitVector.of(0,1).concatenate(significandBV);
        } else {
            significandBV = BitVector.of(1,1).concatenate(significandBV);
        }


        // Compute the unbiased exponent.
        BigInteger bias = BigInteger.valueOf(2).pow( exponentBits.intValue() - 1 ).subtract(BigInteger.ONE);
        long exponent = biasedExponentBV.unsignedValue().longValue()-  bias.longValue();

        // Compute the BigFloat.
        BinaryMathContext mc = new BinaryMathContext(precision.intValue(), exponentBits.intValue());
        return FloatToken.of(new BigFloat(sign,
                significandBV.unsignedValue(), exponent, mc),
                exponentBits.intValue());
    }

    /**
     * float2mint converts a float point value (single or double precision) {@code term} to a BitVector or MInt
     * of bitwidth {@code bitwidth}.
     */
    public static BitVector float2mint(FloatToken term, IntToken bitwidth, TermContext context) {
        BinaryMathContext mc = getMathContext(term);

        int termPrecision = term.bigFloatValue().precision();
        long termExponent = term.bigFloatValue().exponent(mc.minExponent, mc.maxExponent);
        int termExponentBits = term.exponent();

        // Sanity Checks.
        if(bitwidth.intValue() != 32 && bitwidth.intValue() != 64) {
            throw new IllegalArgumentException("Illegal bitwidth provided: Only 32 or 64 are supported");
        }

        int expectedPrecision = (bitwidth.intValue() == 32)? 24 : 53;
        int expectedExponentBits = (bitwidth.intValue() == 32)? 8 : 11;

        if(termPrecision != expectedPrecision || termExponentBits != expectedExponentBits) {
            throw new IllegalArgumentException("mismatch precision or exponent bits: "
                    + "input floating point precision " + termPrecision + "whereas expected precision based on bitwidth " + expectedPrecision
                    + "input floating point exponent bits " + termExponentBits + "whereas expected exponent bits " + expectedExponentBits);
        }


        BigInteger bias = BigInteger.valueOf(2).pow( termExponentBits - 1 ).subtract(BigInteger.ONE);

        // Compute MInt for sign.
        boolean sign = term.bigFloatValue().sign();
        BitVector signBV = BitVector.of((sign)?1:0,1);

        // Compute MInt for significand.
        BitVector termSignificand = BitVector.of(term.bigFloatValue().significand(mc.minExponent, mc.maxExponent), mc.precision);
        BitVector significandBV = termSignificand.extract(1, mc.precision);

        BigFloat termBigFloat = term.bigFloatValue();
        // Case I: positive/negative zero and sub-normals
        if(termBigFloat.isNegativeZero() || termBigFloat.isPositiveZero() || termBigFloat.isSubnormal(mc.minExponent)) {
            BitVector exponentBV = BitVector.of(0, termExponentBits);
            return signBV.concatenate(exponentBV.concatenate(significandBV));
        }

        // Case II: Nan and Infinite(Positive or Negative)
        if(termBigFloat.isNaN() || termBigFloat.isInfinite()) {
            BitVector exponentBV = BitVector.of(-1, termExponentBits);
            return signBV.concatenate(exponentBV.concatenate(significandBV));
        }

        // Case III: Normalized values
        BitVector exponentBV = BitVector.of(termExponent + bias.longValue(), termExponentBits);
        return signBV.concatenate(exponentBV.concatenate(significandBV));
    }

    public static FloatToken ceil(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().rint(getMathContext(term)
                .withRoundingMode(RoundingMode.CEILING)), term.exponent());
    }

    public static FloatToken floor(FloatToken term, TermContext context) {
        return FloatToken.of(term.bigFloatValue().rint(getMathContext(term)
                .withRoundingMode(RoundingMode.FLOOR)), term.exponent());
    }

    public static BoolToken isNaN(FloatToken term, TermContext context) {
        return BoolToken.of(term.bigFloatValue().isNaN());
    }

    public static FloatToken maxValue(IntToken precision, IntToken exponentBits, TermContext context) {
        BinaryMathContext mc = new BinaryMathContext(precision.intValue(), exponentBits.intValue());
        return FloatToken.of(BigFloat.maxValue(mc.precision, mc.maxExponent), exponentBits.intValue());
    }

    public static FloatToken minValue(IntToken precision, IntToken exponentBits, TermContext context) {
        BinaryMathContext mc = new BinaryMathContext(precision.intValue(), exponentBits.intValue());
        return FloatToken.of(BigFloat.minValue(mc.precision, mc.minExponent), exponentBits.intValue());
    }
}
