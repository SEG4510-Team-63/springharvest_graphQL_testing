package client;

/**
 * Transformer interface for customizing
 * filter field and value.
 *
 * @author sjaiswal on 11/18/21
 */
public interface FieldValueTransformer {

    /**
     * Returns a new field name for the given field name.
     * @param fieldName
     * @return
     */
    public String transformField(String fieldName);

    /**
     * Returns an instance of FieldValuePair.
     * @param fieldName
     * @param value
     * @return
     */
    public FieldValuePair<? extends Object> transformValue(String fieldName, Object value);
}