package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.shared.constants.*;
import graphql.schema.DataFetchingFieldSelectionSet;
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
import org.springframework.test.util.ReflectionTestUtils;

import java.io.Serializable;
import java.util.*;

public class AbstractGraphQLCrudControllerTest {

    private class AuthorDTO{
        public String name;
        public UUID id;
        public PetDTO pet;
    }
    private class PetDTO{
        public String name;
        public UUID id;

    }
    private TypedQueryBuilder typedQueryBuilder = mock(TypedQueryBuilder.class);
    private AbstractGraphQLCrudController<BaseEntity<Serializable>, Serializable> abstractGraphQLCrudController = mock(AbstractGraphQLCrudController.class);
    private Class<BaseEntity> entityClass = BaseEntity.class;
    private Class<Serializable> keyClass = Serializable.class;

    @Test
    void searchWithValidFilter() {
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        when(environment.getSelectionSet()).thenReturn(mock(DataFetchingFieldSelectionSet.class));
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());
        PageData<BaseEntity<Serializable>> expectedPageData = new PageData<>(new ArrayList<>(), 1, 10, 0L, 1, 10);
        PageData<BaseEntity<Serializable>> another = new PageData<>(new ArrayList<>(), 1, 10, 1L, 1, 10);

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging)))
                .thenReturn(expectedPageData);

        PageData<BaseEntity<Serializable>> result = abstractGraphQLCrudController.search(filter, clause, paging, environment);

        assertEquals(expectedPageData, result);
    }

    @Test
    void searchWithNullFilter() {
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        when(environment.getSelectionSet()).thenReturn(mock(DataFetchingFieldSelectionSet.class));
        Map<String, Object> filter = null;
        Map<String, Object> clause = new HashMap<>();
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging)))
                .thenReturn(null);

        PageData<BaseEntity<Serializable>> result = abstractGraphQLCrudController.search(filter, clause, paging, environment);

        assertNotNull(result);
    }

    @Test
    void searchWithEmptyFilter() {
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        when(environment.getSelectionSet()).thenReturn(mock(DataFetchingFieldSelectionSet.class));
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        DataPaging paging = new DataPaging(1, 10, new ArrayList<>());
        PageData<BaseEntity<Serializable>> expectedPageData = new PageData<>(new ArrayList<>(), 1, 10, 0L, 1, 10);

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging)))
                .thenReturn(expectedPageData);

        PageData<BaseEntity<Serializable>> result = abstractGraphQLCrudController.search(filter, clause, paging, environment);

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
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = Arrays.asList("field1", "field2");
        long expectedCount = 0L;

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(fields), anyMap(), any(), any()))
                .thenReturn(expectedCount);

        ReflectionTestUtils.setField(abstractGraphQLCrudController, "typedQueryBuilder", typedQueryBuilder);
        long result = abstractGraphQLCrudController.count(filter, clause, fields);

        assertEquals(expectedCount, result);
    }

    @Test
    void countWithEmptyFilter() {
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = new ArrayList<>();
        long expectedCount = 0L;

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(fields), anyMap(), any(), any()))
                .thenReturn(expectedCount);

        long result = abstractGraphQLCrudController.count(filter, clause, fields);

        assertEquals(expectedCount, result);
    }
}