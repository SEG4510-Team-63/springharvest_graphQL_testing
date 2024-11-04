package dev.springharvest.expression.unit;

import dev.springharvest.expressions.ast.Operation;
import dev.springharvest.expressions.helpers.JpaTypedQueryBuilder;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.mockito.Mockito.when;

@ExtendWith(SpringExtension.class)
public class JpaTypedQueryBuilderTest {

    @InjectMocks
    private JpaTypedQueryBuilder queryBuilder;

    @Mock
    private EntityManagerFactory entityManagerFactory;

    @Mock
    private EntityManager entityManager;

    @Mock
    private CriteriaBuilder criteriaBuilder;

    @Mock
    private CriteriaQuery<Object> criteriaQuery;

    @Mock
    private Root<Object> root;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
        when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
        when(criteriaBuilder.createQuery(Object.class)).thenReturn(criteriaQuery);
    }

    @Test
    public void testParseFilterExpression_withValidInput() {
        Map<String, Object> filterMap = Map.of("name", "TestName");
        Map<String, Object> operationMap = Map.of("operation", "equals");
        List<String> fields = List.of("name");
        Operation operation = Operation.SEARCH;

        when(criteriaQuery.from(Object.class)).thenReturn(root);

        // Act
        List<Object> result = queryBuilder.parseFilterExpression(filterMap, operationMap, Object.class, fields, operation);

        // Assert
        Assertions.assertNotNull(result);
        // Add assertions specific to the behavior you expect, such as checking the contents of `result`
    }

    @Test
    public void testParseFilterExpression_withNullFilterAndOperationMaps() {
        // Arrange
        Map<String, Object> filterMap = null;
        Map<String, Object> operationMap = null;
        List<String> fields = List.of("name");
        Operation operation = Operation.SEARCH;

        when(criteriaQuery.from(Object.class)).thenReturn(root);

        // Act
        List<Object> result = queryBuilder.parseFilterExpression(null, null, Object.class, fields, operation);

        // Assert
        Assertions.assertNotNull(result);
        // Verify expected behavior when no filters or operations are provided (e.g., empty result or default handling)
    }

    @Test
    void testParseFilterExpression_withEmptyFilterMap() {
        // Arrange
        Map<String, Object> filterMap = Collections.emptyMap();
        Map<String, Object> operationMap = Map.of("operation", "equals");
        List<String> fields = List.of("name");
        Operation operation = Operation.SEARCH;

        when(criteriaQuery.from(Object.class)).thenReturn(root);

        // Act
        List<Object> result = queryBuilder.parseFilterExpression(filterMap, operationMap, Object.class, fields, operation);

        // Assert
        Assertions.assertNotNull(result);
        // Check if the operation is applied despite an empty filter map
    }

    @Test
    void testParseFilterExpression_withMultipleFilters() {
        // Arrange
        Map<String, Object> filterMap = Map.of("name", "TestName", "age", 25);
        Map<String, Object> operationMap = Map.of("name", "equals", "age", "greaterThan");
        List<String> fields = List.of("name", "age");
        Operation operation = Operation.SEARCH;

        when(criteriaQuery.from(Object.class)).thenReturn(root);

        // Act
        List<Object> result = queryBuilder.parseFilterExpression(filterMap, operationMap, Object.class, fields, operation);

        // Assert
        Assertions.assertNotNull(result);
        // Verify that the result matches expectations based on multiple filter criteria
    }

    @Test
    void testParseFilterExpression_withInvalidFilterKey() {
        // Arrange
        Map<String, Object> filterMap = Map.of("nonExistentField", "value");
        Map<String, Object> operationMap = Map.of("nonExistentField", "equals");
        List<String> fields = List.of("name");
        Operation operation = Operation.SEARCH;

        when(criteriaQuery.from(Object.class)).thenReturn(root);

        // Act & Assert
        assertThrows(IllegalArgumentException.class, () -> {
            queryBuilder.parseFilterExpression(filterMap, operationMap, Object.class, fields, operation);
        });
    }
}
