package dev.springharvest.library.global.expressions;

import java.time.LocalDate;
import java.util.List;

public record DateExpression(LocalDate eq,
                             LocalDate gt,
                             LocalDate gte,
                             LocalDate lt,
                             LocalDate lte,
                             List<LocalDate> between) {
}
