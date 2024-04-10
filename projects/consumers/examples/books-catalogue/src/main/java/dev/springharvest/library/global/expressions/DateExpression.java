package dev.springharvest.library.global.expressions;

import java.time.LocalDate;
import java.util.List;

// This class is used to represent a date expression
// It contains various fields to represent different date conditions
public record DateExpression(
    LocalDate eq, // Represents the 'equal to' condition
    LocalDate gt, // Represents the 'greater than' condition
    LocalDate gte, // Represents the 'greater than or equal to' condition
    LocalDate lt, // Represents the 'less than' condition
    LocalDate lte, // Represents the 'less than or equal to' condition
    List<LocalDate> between // Represents a range of dates
) {}