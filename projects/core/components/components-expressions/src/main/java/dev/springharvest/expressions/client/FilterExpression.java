package dev.springharvest.expressions.client;

import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import dev.springharvest.expressions.ast.Expression;
import dev.springharvest.expressions.visitors.ExpressionVisitor;
//import graphql.language.Field;
import java.lang.reflect.Field;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.data.jpa.domain.Specification;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class is used by clients of this
 * library to get the target filter expression
 * of specified format.
 *
 * @author sjaiswal
 * @author NeroNemesis
 */
public class FilterExpression {

    //private K param;
    private Map<String,String> fieldMap;
    private Expression expressionAst;
    private FieldValueTransformer fieldValueTransformer;

    private FilterExpression(FilterExpressionBuilder expressionBuilder) {
        //this.param = expressionBuilder.param;
        this.fieldMap = expressionBuilder.fieldMap;
        this.expressionAst = expressionBuilder.expressionAst;
        this.fieldValueTransformer = expressionBuilder.fieldValueTransformer;
    }

    /**
     * Builder class responsible for building the
     * instance of FilterExpression class.
     */
    public static class FilterExpressionBuilder {

        //private K param;
        private Map<String,String> fieldMap;
        private Expression expressionAst;
        private Map args;
        private FieldValueTransformer fieldValueTransformer;

        private FilterExpressionBuilder () {
            fieldMap = new HashMap<>();
        }

        //public FilterExpressionBuilder<K> param(K params) {
            //this.param = params;
            //return this;
        //}

        public FilterExpressionBuilder map(String source, String target) {
            fieldMap.put(source,target);
            return this;
        }

        public FilterExpressionBuilder map(Map<String, String> fieldMap) {
            this.fieldMap = fieldMap;
            return this;
        }

        public FilterExpressionBuilder args(Map args) {
            this.args = args;
            return this;
        }

        public FilterExpressionBuilder transform(FieldValueTransformer fieldValueTransformer) {
            this.fieldValueTransformer = fieldValueTransformer;
            return this;
        }

        public FilterExpression build() {
            FilterExpressionParser expressionParser = new FilterExpressionParser();
            if (args != null) {
                expressionAst = expressionParser.parseFilterExpression(args);
            }
            FilterExpression expression = new FilterExpression(this);
            return expression;
        }
    }

    public static FilterExpressionBuilder newFilterExpressionBuilder() {
        return new FilterExpressionBuilder();
    }

    /**
     * This method returns the expression in
     * required format.
     * @param format
     * @param <T>
     * @return
     */
    public <T> T getExpression(ExpressionFormat format) {
        if (expressionAst == null) {
            throw new InvalidFilterException("Missing or invalid filter arguments");
        }
        ExpressionVisitor<T> expressionVisitor = ExpressionVisitorFactory.getExpressionVisitor(format, fieldMap, fieldValueTransformer);
        return expressionVisitor.expression(expressionAst);
    }
}
