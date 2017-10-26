// Copyright (c) 2013-2016 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;

import org.apache.commons.lang3.StringUtils;
import org.kframework.backend.java.kil.Sort;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.backend.java.kil.Token;
import org.kframework.kil.FloatBuiltin;
import org.kframework.utils.StringUtil;

import java.math.BigInteger;

/**
 *
 * @author: ali
 */

public class BuiltinChecksumOperations {

    public static IntToken csum16(IntToken number, IntToken inWidth, IntToken outWidth, TermContext context) {
        // based on http://web.eecs.utk.edu/~cs594np/unp/checksum.html
        //ignoring both input and output width
        byte[] bytes = number.bigIntegerValue().toByteArray();
        int sum  = 0;
        int len = bytes.length;
        while (len > 1){
            sum += Byte.toUnsignedInt(bytes[len-1]) + (Byte.toUnsignedInt(bytes[len-2]) << 8);
            if ((sum & 0x80000000) != 0)
                sum = (sum & 0xFFFF) + (sum >> 16);
            len -= 2;
        }
        if (len > 0)
            sum += Byte.toUnsignedInt(bytes[0]);

        while ((sum >> 16) != 0)
            sum = (sum & 0xFFFF) + (sum >> 16);

        sum = ~sum;

        return IntToken.of(BigInteger.valueOf(sum));

    }
}
