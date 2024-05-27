package dev.springharvest.expressions.client;

/**
 * Supported expression formats.
 *
 * @author sjaiswal
 * @author NeroNemesis
 */
public enum ExpressionFormat {

    JPA("JPA"),
    ELASTICSEARCH("ELASTICSEARCH");

    private String type;
    ExpressionFormat(String type) {
        this.type = type;
    }

    public static ExpressionFormat getFormat(String type) {
        for (ExpressionFormat format : ExpressionFormat.values()) {
            if (format.type.equals(type)) {
                return format;
            }
        }
        return JPA;
    }
}
