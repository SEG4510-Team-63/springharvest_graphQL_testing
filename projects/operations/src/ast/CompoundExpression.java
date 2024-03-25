package ast;

/*This import error isn't resolved yet. Need to find an import path to go to 'visitor' folder to import ExpressionVisitor*/
import visitor.ExpressionVisitor;

public class CompoundExpression extends AbstractExpression{
    /**
     * Constructor to create a compound expression node
     * with left operand, operator and right operand.
     * @param leftOperand
     * @param operator
     * @param rightOperand
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
