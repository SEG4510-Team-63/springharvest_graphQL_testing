package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.shared.constants.*;
import graphql.schema.DataFetchingFieldSelectionSet;
import jakarta.persistence.criteria.JoinType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import dev.springharvest.expressions.builders.TypedQueryBuilder;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import dev.springharvest.expressions.helpers.Operation;
import graphql.schema.DataFetchingEnvironment;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.Serializable;
import java.util.*;

public class AbstractGraphQLCrudControllerTest {

    private class Author extends BaseEntity<UUID>{
        public String name;
        public Pet pet;
    }
    private class Pet extends BaseEntity<UUID>{
        public String name;
    }

    @Mock
    private TypedQueryBuilder typedQueryBuilder;
    private Class<Author> entityClass = Author.class;
    private Class<UUID> keyClass = UUID.class;

    @Autowired
    private AbstractGraphQLCrudController<Author, UUID> abstractGraphQLCrudController;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        abstractGraphQLCrudController = new AbstractGraphQLCrudController<>(Author.class, UUID.class) {};
        abstractGraphQLCrudController.setTypedQueryBuilder(typedQueryBuilder);
    }

    @Test
    void searchWithValidFilter() {
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        DataFetchingFieldSelectionSet selectionSet = mock(DataFetchingFieldSelectionSet.class);
        when(environment.getSelectionSet()).thenReturn(selectionSet);
        when(selectionSet.getFields()).thenReturn(Collections.emptyList());
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        Map<String, JoinType> joins = new HashMap<>();
        List<String> fields = new ArrayList<>();
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());
        PageData<Author> expectedPageData = new PageData<>(new ArrayList<>(), 1, 10, 0L, 1, 10);
        //PageData<AuthorDTO> another = new PageData<>(new ArrayList<>(), 1, 10, 1L, 1, 10);

        when(typedQueryBuilder.parseFilterExpression(
                eq(Operation.SEARCH),
                eq(entityClass),
                eq(keyClass),
                eq(filter),
                eq(clause),
                eq(fields),
                eq(joins),
                eq(null),
                eq(paging)
        )).thenReturn(expectedPageData);

        PageData<Author> result = abstractGraphQLCrudController.search(filter, clause, paging, environment);

        assertEquals(expectedPageData, result);
    }

    @Test
    void searchWithNullFilter() {
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        when(environment.getSelectionSet()).thenReturn(mock(DataFetchingFieldSelectionSet.class));
        Map<String, Object> filter = null;
        Map<String, Object> clause = new HashMap<>();
        clause.put("distinct", "true");
        // You are missing test cases
        // You need to test with a joins Map, field, and aggregates object too
        // Refer to BooksGraphQLControllerTest.java for joins Map examples
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging)))
                .thenReturn(null);

        PageData<Author> result = abstractGraphQLCrudController.search(filter, clause, paging, environment);

        assertNotNull(result);
    }

    @Test
    void searchWithEmptyFilter() {
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        when(environment.getSelectionSet()).thenReturn(mock(DataFetchingFieldSelectionSet.class));
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());
        PageData<Author> expectedPageData = new PageData<>(new ArrayList<>(), 1, 10, 0L, 1, 10);

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging)))
                .thenReturn(expectedPageData);

        PageData<Author> result = abstractGraphQLCrudController.search(filter, clause, paging, environment);

        assertNotNull(result);
        assertEquals(expectedPageData, result);
    }

    @Test
    void searchWithAggregatesFilter() {
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        Aggregates aggregatesFilter = new Aggregates(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());
        Object expectedResult = new Object();

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(fields), anyMap(), any(), eq(paging)))
                .thenReturn(expectedResult);

        Object result = abstractGraphQLCrudController.search(filter, clause, fields, aggregatesFilter, paging);

        assertNotNull(result);
        assertEquals(expectedResult, result);
    }

    @Test
    void countWithValidFilter() {
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> equals = new LinkedHashMap<>();
        equals.put("containsic", "ss");
        filter.put("name", equals);
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = List.of("Author_name");
        List<String> formattedFields = List.of("Author.name");
        long expectedCount = 4L;

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(formattedFields), eq(null), eq(null), eq(null)))
                .thenReturn(expectedCount);

        long result = abstractGraphQLCrudController.count(filter, clause, fields);

        assertEquals(expectedCount, result);
    }

    @Test
    void countWithEmptyFilter() {
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = List.of("Author_id");
        List<String> formattedFields = List.of("Author.id");
        long expectedCount = 0L;

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(formattedFields), eq(null), eq(null), eq(null)))
                .thenReturn(expectedCount);

        long result = abstractGraphQLCrudController.count(filter, clause, fields);

        assertEquals(expectedCount, result);
    }
}