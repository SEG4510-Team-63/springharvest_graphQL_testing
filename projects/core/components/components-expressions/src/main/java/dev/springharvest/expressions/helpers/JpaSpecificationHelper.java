package dev.springharvest.expressions.helpers;

import dev.springharvest.expressions.client.ExpressionFormat;
import dev.springharvest.expressions.client.FilterExpression;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;

import java.util.Map;

/**
 * This class is responsible to help build
 * JPA Specification from a filter map
 *
 * @author NeroNemesis
 */

public class JpaSpecificationHelper {
    public static <T, R> Specification<T> createJoinSpecification(String tableName, String PropertyName, R filterValue) {
        return (root, criteriaQuery, criteriaBuilder) -> {
            From<?, ?> from = root;
            Path<R> path = from.join(tableName.strip()).get(PropertyName.strip()); // Assuming the related property is always "name"
            Predicate predicate = criteriaBuilder.equal(path, filterValue);
            return predicate;
        };
    }

    public static <T> Specification<T> parseFilterExpression(Map filterMap) {
        FilterExpression.FilterExpressionBuilder builder = FilterExpression.newFilterExpressionBuilder();
        FilterExpression filterExpression = builder.args(filterMap).build();
        Specification<T> spec = filterExpression.getExpression(ExpressionFormat.JPA);
        return spec;
    }
}
