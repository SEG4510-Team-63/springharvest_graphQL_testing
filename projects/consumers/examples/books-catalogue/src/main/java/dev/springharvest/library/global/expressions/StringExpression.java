package dev.springharvest.library.global.expressions;

import java.util.List;

// This class is used to represent a string expression
// It contains various fields to represent different string conditions
public record StringExpression(
    String equals, // Represents the 'equal to' condition
    String equalsic, // Represents the 'equal to' condition (case insensitive)
    String contains, // Represents the 'contains' condition
    String constainsic, // Represents the 'contains' condition (case insensitive)
    List<String> in, // Represents a list of strings to match
    String starts, // Represents the 'starts with' condition
    String startsic, // Represents the 'starts with' condition (case insensitive)
    String ends, // Represents the 'ends with' condition
    String endsic // Represents the 'ends with' condition (case insensitive)
) {}