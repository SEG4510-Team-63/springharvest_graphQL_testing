package dev.springharvest.library.global.expressions;

import java.util.List;

public record StringExpression(String equals,
                               String equalsic,
                               String contains,
                               String constainsic,
                               List<String> in,
                               String starts,
                               String startsic,
                               String ends,
                               String endsic) {}
