package client;

/**
 * Supported expression formats.
 *
 * @author sjaiswal
 */
public enum ExpressionFormat {

    SQL("SQL"),
    INFIX("INFIX"),
    JPA("JPA"),
    MONGO("MONGO"),
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
        return SQL;
    }
}