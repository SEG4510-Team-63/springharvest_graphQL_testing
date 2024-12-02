package dev.springharvest.expressions.builders;

import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.shared.constants.*;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.Tuple;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class TypedQueryBuilderTest {

    private EntityManagerFactory entityManagerFactory;
    private EntityManager entityManager;
    private CriteriaBuilder criteriaBuilder;
    private CriteriaQuery<Tuple> criteriaQuery;
    private Root<Object> root;
    private TypedQueryBuilder typedQueryBuilder;

    @BeforeEach
    void setUp() {
        entityManagerFactory = mock(EntityManagerFactory.class);
        entityManager = mock(EntityManager.class);
        criteriaBuilder = mock(CriteriaBuilder.class);
        criteriaQuery = mock(CriteriaQuery.class);
        root = mock(Root.class);

        when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
        when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
        when(criteriaBuilder.createQuery(Tuple.class)).thenReturn(criteriaQuery);
        when(criteriaQuery.from(Object.class)).thenReturn(root);

        typedQueryBuilder = new TypedQueryBuilder();
        ReflectionTestUtils.setField(typedQueryBuilder, "entityManagerFactory", entityManagerFactory);
    }

    @Test
    void parseFilterExpressionWithValidFilter() {
        Map<String, Object> filterMap = new HashMap<>();
        filterMap.put("name", "test");
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregatesFilter = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filterMap, clauseMap, fields, joins, aggregatesFilter, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpressionWithNullFilter() {
        Map<String, Object> filterMap = null;
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregatesFilter = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filterMap, clauseMap, fields, joins, aggregatesFilter, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpressionWithEmptyFilter() {
        Map<String, Object> filterMap = new HashMap<>();
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregatesFilter = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filterMap, clauseMap, fields, joins, aggregatesFilter, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpressionWithAggregatesFilter() {
        Map<String, Object> filterMap = new HashMap<>();
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Aggregates aggregatesFilter = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filterMap, clauseMap, fields, new HashMap<>(), aggregatesFilter, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpressionWithCountOperation() {
        Map<String, Object> filterMap = new HashMap<>();
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregatesFilter = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.COUNT, Object.class, Object.class, filterMap, clauseMap, fields, joins, aggregatesFilter, paging);

        assertNotNull(result);
    }


}