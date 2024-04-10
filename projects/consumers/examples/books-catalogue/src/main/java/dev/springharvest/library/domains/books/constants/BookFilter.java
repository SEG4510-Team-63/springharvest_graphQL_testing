package dev.springharvest.library.domains.books.constants;

import dev.springharvest.library.global.expressions.DateExpression;
import dev.springharvest.library.global.expressions.StringExpression;

import java.util.List;

public record BookFilter(StringExpression title,
                        StringExpression genre,
                        DateExpression dateUpdated,
                        List<BookFilter> and,
                        List<BookFilter> or,
                        BookFilter not) {
}
