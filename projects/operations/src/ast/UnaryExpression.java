package ast;

/*This import error isn't resolved yet. Need to find an import path to go to 'visitor' folder to import ExpressionVisitor*/
import visitor.ExpressionVisitor;

/**
 * This class represents an UnaryExpression node
 * in the expression tree with just NOT operator
 * and a single operand which can either be a
 * BinaryExpression or CompoundExpression.
 *
 * @author sjaiswal
 */
public class UnaryExpression extends AbstractExpression{

    /**
     * Default constructor.
     */
    public UnaryExpression() {
        super();
    }

    /**
     * Parameterized constructor.
     *
     * @param leftOperand
     * @param operator
     * @param rightOperand
     */
    public UnaryExpression(Expression leftOperand, Operator operator, Expression rightOperand) {
        super(leftOperand, operator, rightOperand);
    }

    /**
     * This method accepts a expression visitor and calls
     * the visit method on the visitor passing itself.
     *
     * @param visitor ExpressionVisitor for traversing the expression tree.
     * @param data    Buffer to hold the result of process node.
     * @return Returns the processed data.
     */
    @Override
    public <T> T accept(ExpressionVisitor visitor, T data) {
        return (T)visitor.visitUnaryExpression(this, data);
    }
}
