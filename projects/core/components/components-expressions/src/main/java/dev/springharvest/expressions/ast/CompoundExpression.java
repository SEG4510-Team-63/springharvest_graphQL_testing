package dev.springharvest.expressions.ast;

import dev.springharvest.expressions.visitors.ExpressionVisitor;

public class CompoundExpression extends AbstractExpression{
    /**
     * Constructor to create a compound expression node
     * with left operand, operator and right operand.
     * @param leftOperand
     * @param operator
     * @param rightOperand
     *
     * @author sjaiswal
     */
    public CompoundExpression(Expression leftOperand, Operator operator, Expression rightOperand) {
        super(leftOperand,operator,rightOperand);
    }

    /**
     * Default constructor.
     */
    public CompoundExpression() {
        super();
    }

    /**
     * This method accepts a expression visitor and calls
     * the visit method on the visitor passing itself.
     * @param visitor
     *        ExpressionVisitor for traversing the expression tree.
     * @param data
     *        Buffer to hold the result of process node.
     * @param <T>
     * @return
     *        Returns the processed data.
     */
    @Override
    public <T> T accept(ExpressionVisitor visitor, T data) {
        return (T)visitor.visitCompoundExpression(this, data);
    }
}
