package dev.springharvest.library.global.expressions;

import java.util.List;

// This class is used to represent an integer expression
// It contains various fields to represent different integer conditions
public record IntExpression(
    int eq, // Represents the 'equal to' condition
    int gt, // Represents the 'greater than' condition
    int gte, // Represents the 'greater than or equal to' condition
    int lt, // Represents the 'less than' condition
    int lte, // Represents the 'less than or equal to' condition
    List<Integer> in, // Represents a list of integers to match
    List<Integer> between // Represents a range of integers
) {}