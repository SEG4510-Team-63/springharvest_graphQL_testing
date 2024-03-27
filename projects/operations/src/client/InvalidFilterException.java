package client;

/**
 * Exception for an invalid expression.
 *
 * @author sjaiswal
 */
public class InvalidFilterException extends RuntimeException{

    public InvalidFilterException(String message) {
        super(message);
    }
}
