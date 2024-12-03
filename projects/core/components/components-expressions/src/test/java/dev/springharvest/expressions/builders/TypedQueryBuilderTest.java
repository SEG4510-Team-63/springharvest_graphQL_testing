package dev.springharvest.expressions.builders;

import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.shared.constants.Aggregates;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.Sort;
import dev.springharvest.shared.constants.SortDirection;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.Tuple;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class TypedQueryBuilderTest {

    private class Author extends BaseEntity<UUID> {
        public String name;
        public Pet pet;
    }
    private class Pet extends BaseEntity<UUID>{
        public String name;
    }

    @Mock
    private EntityManagerFactory entityManagerFactory;

    @Mock
    private EntityManager entityManager;

    @Mock
    private CriteriaBuilder criteriaBuilder;

    @Mock
    private CriteriaQuery<Tuple> criteriaQuery;

    @Mock
    private Root<Object> root;

    @InjectMocks
    private TypedQueryBuilder typedQueryBuilder;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        typedQueryBuilder = new TypedQueryBuilder() {};
        typedQueryBuilder.setEntityManagerFactory(entityManagerFactory);
    }

    @Test
    void parseFilterExpression_withValidInputs_returnsExpectedResult() {
        Map<String, Object> filterMap = new HashMap<>();
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregates = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, Collections.emptyList());

        when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
        when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
        when(criteriaBuilder.createQuery(Tuple.class)).thenReturn(criteriaQuery);
        when(criteriaQuery.from(Object.class)).thenReturn(root);

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filterMap, clauseMap, fields, joins, aggregates, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpression_withNullFilterMap_returnsExpectedResult() {
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregates = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, Collections.emptyList());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, null, clauseMap, fields, joins, aggregates, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpression_withEmptyFilterMap_returnsExpectedResult() {
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = new ArrayList<>();
        Map<String, JoinType> joins = new HashMap<>();
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());

        when(entityManagerFactory.createEntityManager()).thenReturn(entityManager);
        when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
        when(criteriaBuilder.createQuery(Tuple.class)).thenReturn(criteriaQuery);
        when(criteriaQuery.from(Object.class)).thenReturn(root);

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filter, clause, fields, joins, null, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpression_withNullFields_returnsExpectedResult() {
        Map<String, Object> filterMap = new HashMap<>();
        Map<String, Object> clauseMap = new HashMap<>();
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregates = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, Collections.emptyList());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filterMap, clauseMap, null, joins, aggregates, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpression_withNullPaging_returnsExpectedResult() {
        Map<String, Object> filterMap = new HashMap<>();
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregates = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filterMap, clauseMap, fields, joins, aggregates, null);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpression_withCountOperation_returnsExpectedResult() {
        Map<String, Object> filterMap = new HashMap<>();
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregates = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, Collections.emptyList());

        Object result = typedQueryBuilder.parseFilterExpression(Operation.COUNT, Object.class, Object.class, filterMap, clauseMap, fields, joins, aggregates, paging);

        assertNotNull(result);
    }

    @Test
    void parseFilterExpression_withNullEntityManager_throwsException() {
        when(entityManagerFactory.createEntityManager()).thenReturn(null);

        Map<String, Object> filterMap = new HashMap<>();
        Map<String, Object> clauseMap = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Map<String, JoinType> joins = new HashMap<>();
        Aggregates aggregates = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, Collections.emptyList());

        assertThrows(NullPointerException.class, () -> {
            typedQueryBuilder.parseFilterExpression(Operation.SEARCH, Object.class, Object.class, filterMap, clauseMap, fields, joins, aggregates, paging);
        });
    }
}