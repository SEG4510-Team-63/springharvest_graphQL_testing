package dev.springharvest.library.global.expressions;

import java.time.LocalDate;
import java.util.List;

public record IntExpression(int eq,
                            int gt,
                            int gte,
                            int lt,
                            int lte,
                            List<Integer> in,
                            List<Integer> between) {
}
