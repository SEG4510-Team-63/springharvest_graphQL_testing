package client;

/*Error on importing since lacking visitor package*/
import visitors.ElasticsearchCriteriaExpressionVisitor;
import visitors.ExpressionVisitor;
import visitors.InfixExpressionVisitor;
import visitors.JpaSpecificationExpressionVisitor;
import visitors.MongoCriteriaExpressionVisitor;
import cvisitors.SQLExpressionVisitor;

import java.util.Map;

/**
 * Expression visitor factory responsible
 * for creating instances of supported
 * expression visitors.
 *
 * @author sjaiswal
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
        ExpressionVisitor expressionVisitor = new InfixExpressionVisitor(fieldMap, fieldValueTransformer);
        if (format != null) {
            switch (format) {
                case INFIX:
                    expressionVisitor = new InfixExpressionVisitor(fieldMap, fieldValueTransformer);
                    break;
                case SQL:
                    expressionVisitor =  new SQLExpressionVisitor(fieldMap, fieldValueTransformer);
                    break;
                case JPA:
                    expressionVisitor = new JpaSpecificationExpressionVisitor(fieldMap, fieldValueTransformer);
                    break;
                case MONGO:
                    expressionVisitor =  new MongoCriteriaExpressionVisitor(fieldMap, fieldValueTransformer);
                    break;
                case ELASTICSEARCH:
                    expressionVisitor = new ElasticsearchCriteriaExpressionVisitor(fieldMap, fieldValueTransformer);
                    break;
            }
        }
        return expressionVisitor;
    }
}
