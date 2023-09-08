package dev.springharvest.library.authors.constants;

import dev.springharvest.errors.constants.ExceptionMessages;

public class AuthorConstants {

    private AuthorConstants() {
        throw new UnsupportedOperationException(ExceptionMessages.PRIVATE_CONSTRUCTOR_MESSAGE);
    }

    public static class Controller {
        public static final String TAG = "Authors";
        public static final String DOMAIN_CONTEXT = "/library/authors";

        private Controller() {
            throw new UnsupportedOperationException(ExceptionMessages.PRIVATE_CONSTRUCTOR_MESSAGE);
        }
    }

    public static class Paths {
        public static final String AUTHOR = "author";
        public static final String AUTHORS = "authors";

        private Paths() {
            throw new UnsupportedOperationException(ExceptionMessages.PRIVATE_CONSTRUCTOR_MESSAGE);
        }
    }

}