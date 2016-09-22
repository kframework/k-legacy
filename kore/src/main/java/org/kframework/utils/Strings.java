package org.kframework.utils;

public class Strings {
    /**
     * Takes the value of a KLabel and returns a string representation, delimited with
     * backticks, of the syntax of that KLabel in KORE.
     *
     * Used by the KAST pretty printer.
     *
     * @param str A string value corresponding to a KLabel.
     * @return A string which can be parsed back by a KORE parser to reach the original KLabel.
     */
    public static String escapeKoreKLabel(String value) {
        char delimiter = '`';
        final int length = value.length();
        StringBuilder result = new StringBuilder();
        result.append(delimiter);
        for (int offset = 0, codepoint; offset < length; offset += Character.charCount(codepoint)) {
            codepoint = value.codePointAt(offset);
            if (codepoint == 0x7F || codepoint < 32) {
                throw new IllegalArgumentException("Special characters not supported here:" + value);
            } else if (codepoint == delimiter) {
                result.append("\\" + delimiter);
            } else if (codepoint == '\\') {
                result.append("\\\\");
            } else {
                result.appendCodePoint(codepoint);
            }
        }
        result.append(delimiter);
        return result.toString();
    }

    /**
     * Takes a textual representation of a KLabel using backticks to delimit
     * and returns the string representation of the KLabel that it corresponds to
     *
     * Used by the KAST parser.
     *
     * @param str An image of a parser token corresponding to a KLabel in KORE which
     * begins and ends with backtick
     * @return The string value of the KLabel
     */
    public static String unescapeKoreKLabel(String str) {
        char delimiter = '`';
        StringBuilder sb = new StringBuilder();
        if (str.charAt(0) != delimiter) {
            throw new IllegalArgumentException("Expected to find " + delimiter + " at the beginning of string: " + str);
        }
        if (str.charAt(str.length() - 1) != delimiter) {
            throw new IllegalArgumentException("Expected to find " + delimiter + " at the end of string: " + str);
        }
        for (int i = 1; i < str.length() - 1; i++) {
            if (str.charAt(i) == 0x7F || str.charAt(i) < 32)
                throw new IllegalArgumentException("Special characters not supported here:" + str);
            if (str.charAt(i) == '\\') {
                if (str.charAt(i + 1) == '\\')
                    sb.append('\\');
                else if (str.charAt(i + 1) == delimiter)
                    sb.append(delimiter);
                i++;
            } else
                sb.append(str.charAt(i));
        }

        return sb.toString();
    }
}
