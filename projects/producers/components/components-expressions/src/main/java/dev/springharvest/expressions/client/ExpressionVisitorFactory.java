package dev.springharvest.expressions.client;

import dev.springharvest.expressions.visitors.ElasticsearchCriteriaExpressionVisitor;
import dev.springharvest.expressions.visitors.ExpressionVisitor;
import dev.springharvest.expressions.visitors.JpaSpecificationExpressionVisitor;

import java.util.Map;

/**
 * Expression visitor factory responsible
 * for creating instances of supported
 * expression visitors.
 *
 * @author sjaiswal
 * @author NeroNemesis
 */
class ExpressionVisitorFactory {

    /**
     * Factory method for creating and returning
     * instances of ExpressionVisitor.
     * @param format
     * @param fieldMap
     * @return
     */
    static ExpressionVisitor getExpressionVisitor(ExpressionFormat format,
                                                  Map<String, String> fieldMap,
                                                  FieldValueTransformer fieldValueTransformer) {
        ExpressionVisitor expressionVisitor = new JpaSpecificationExpressionVisitor(fieldMap, fieldValueTransformer);
        if (format != null) {
            switch (format) {
                case JPA:
                    expressionVisitor = new JpaSpecificationExpressionVisitor(fieldMap, fieldValueTransformer);
                    break;
                case ELASTICSEARCH:
                    expressionVisitor = new ElasticsearchCriteriaExpressionVisitor(fieldMap, fieldValueTransformer);
                    break;
            }
        }
        return expressionVisitor;
    }
}
