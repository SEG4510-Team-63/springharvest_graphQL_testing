package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.shared.constants.Sort;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import dev.springharvest.expressions.builders.TypedQueryBuilder;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.PageData;
import dev.springharvest.shared.constants.SortDirection;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import dev.springharvest.expressions.helpers.Operation;
import graphql.schema.DataFetchingEnvironment;
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
    void searchWithValidFilterAndPaging() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", "John");
        Map<String, Object> clause = new HashMap<>();
        DataPaging paging = new DataPaging(0, 10, List.of(new Sort("name", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        List<AuthorDTO> mockAuthors = Arrays.asList(new AuthorDTO(), new AuthorDTO());
        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging)))
                .thenReturn(new PageData<>(mockAuthors, 2, 1, 10, 0, 0));

        PageData<BaseEntity<Serializable>> result = abstractGraphQLCrudController.search(filter, clause, paging, environment);

        assertEquals(2, result.getTotal(), "Expected 2 authors to be returned");
        verify(typedQueryBuilder, times(1)).parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging));
    }

    @Test
    void searchWithEmptyFilterAndPaging() {
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        DataPaging paging = new DataPaging(0, 10, List.of(new Sort("name", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        List<AuthorDTO> mockAuthors = Arrays.asList(new AuthorDTO(), new AuthorDTO());
        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging)))
                .thenReturn(new PageData<>(mockAuthors, 2, 1, 10, 0, 0));

        PageData<BaseEntity<Serializable>> result = abstractGraphQLCrudController.search(filter, clause, paging, environment);

        assertEquals(2, result.getTotal(), "Expected 2 authors to be returned");
        verify(typedQueryBuilder, times(1)).parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(filter), eq(clause), anyList(), anyMap(), any(), eq(paging));
    }

    @Test
    void searchWithNullFilterAndPaging() {
        Map<String, Object> clause = new HashMap<>();
        DataPaging paging = new DataPaging(0, 10, List.of(new Sort("name", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        List<AuthorDTO> mockAuthors = Arrays.asList(new AuthorDTO(), new AuthorDTO());
        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(null), eq(clause), anyList(), anyMap(), any(), eq(paging)))
                .thenReturn(new PageData<>(mockAuthors, 2, 1, 10, 0, 0));

        PageData<BaseEntity<Serializable>> result = abstractGraphQLCrudController.search(null, clause, paging, environment);

        assertEquals(2, result.getTotal(), "Expected 2 authors to be returned");
        verify(typedQueryBuilder, times(1)).parseFilterExpression(eq(Operation.SEARCH), eq(entityClass), eq(keyClass), eq(null), eq(clause), anyList(), anyMap(), any(), eq(paging));
    }

    @Test
    void countWithValidFilter() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", "John");
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = Arrays.asList("name");
        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(fields), eq(null), eq(null), eq(null)))
                .thenReturn(10L);

        long count = abstractGraphQLCrudController.count(filter, clause, fields);

        assertEquals(10L, count, "Expected count to be 10");
        verify(typedQueryBuilder, times(1)).parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(fields), eq(null), eq(null), eq(null));
    }

    @Test
    void countWithEmptyFilter() {
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = Arrays.asList("name");
        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(fields), eq(null), eq(null), eq(null)))
                .thenReturn(10L);

        long count = abstractGraphQLCrudController.count(filter, clause, fields);

        assertEquals(10L, count, "Expected count to be 10");
        verify(typedQueryBuilder, times(1)).parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(filter), eq(clause), eq(fields), eq(null), eq(null), eq(null));
    }

    @Test
    void countWithNullFilter() {
        Map<String, Object> clause = new HashMap<>();
        List<String> fields = Arrays.asList("name");
        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(null), eq(clause), eq(fields), eq(null), eq(null), eq(null)))
                .thenReturn(10L);

        long count = abstractGraphQLCrudController.count(null, clause, fields);

        assertEquals(10L, count, "Expected count to be 10");
        verify(typedQueryBuilder, times(1)).parseFilterExpression(eq(Operation.COUNT), eq(entityClass), eq(keyClass), eq(null), eq(clause), eq(fields), eq(null), eq(null), eq(null));
    }
}