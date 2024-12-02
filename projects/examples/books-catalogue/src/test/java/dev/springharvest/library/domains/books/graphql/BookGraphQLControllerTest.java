package dev.springharvest.library.domains.books.graphql;

import dev.springharvest.library.config.LiquibaseTestExecutionListener;
import dev.springharvest.library.config.TestComponentScanningConfig;
import dev.springharvest.library.config.TestContainerConfig;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import dev.springharvest.library.domains.books.services.BookCrudService;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.shared.constants.*;
import jakarta.persistence.criteria.JoinType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Map;
import java.util.List;
import java.util.Collections;
import java.util.UUID;

import graphql.schema.DataFetchingEnvironment;
import graphql.schema.DataFetchingFieldSelectionSet;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;

import dev.springharvest.library.domains.books.graphql.BookGraphQLController;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.expressions.builders.TypedQueryBuilder;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(value = {TestComponentScanningConfig.class, TestContainerConfig.class})
@TestExecutionListeners(
        listeners = {DependencyInjectionTestExecutionListener.class, LiquibaseTestExecutionListener.class},
        mergeMode = TestExecutionListeners.MergeMode.MERGE_WITH_DEFAULTS)
@TestPropertySource(locations = "classpath:application.properties")
@ActiveProfiles("test")
@AutoConfigureMockMvc
class BookGraphQLControllerTest {

    @Autowired
    private BookGraphQLController bookGraphQLController;

    @MockBean
    private BookCrudService booksCrudService;

    @MockBean
    private TypedQueryBuilder typedQueryBuilder;

    @Test
    void searchBooksReturnsExpectedResults() {
        Map<String, Object> filter = Map.of("title", "Effective Java");
        Map<String, Object> clause = Map.of();
        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("title", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);

        DataFetchingFieldSelectionSet selectionSet = mock(DataFetchingFieldSelectionSet.class);
        when(environment.getSelectionSet()).thenReturn(selectionSet);
        when(selectionSet.getFields()).thenReturn(Collections.emptyList());

        List<BookEntity> books = List.of(BookEntity.builder().title("Effective Java").build());
        PageData<BookEntity> expectedPageData = new PageData<>(books, 1, 1, 10, 1, 10);

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(BookEntity.class), eq(UUID.class), eq(filter), eq(clause), anyList(), anyMap(), isNull(), eq(paging)))
                .thenReturn(expectedPageData);

        PageData<BookEntity> result = bookGraphQLController.searchBooks(filter, clause, paging, environment);

        assertEquals(expectedPageData, result);
    }

    @Test
    void searchBooksReturnsEmptyResults() {
        Map<String, Object> filter = Map.of("title", "NonExistentBook");
        Map<String, Object> clause = Map.of();
        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("title", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);

        DataFetchingFieldSelectionSet selectionSet = mock(DataFetchingFieldSelectionSet.class);
        when(environment.getSelectionSet()).thenReturn(selectionSet);
        when(selectionSet.getFields()).thenReturn(Collections.emptyList());

        PageData<BookEntity> expectedPageData = new PageData<>(List.of(), 0, 0, 10, 1, 10);

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(BookEntity.class), eq(UUID.class), eq(filter), eq(clause), anyList(), anyMap(), isNull(), eq(paging)))
                .thenReturn(expectedPageData);

        PageData<BookEntity> result = bookGraphQLController.searchBooks(filter, clause, paging, environment);

        assertEquals(expectedPageData, result);
    }

//    @Test
//    void complexBooksSearchReturnsExpectedResults() {
//        Map<String, Object> filter = Map.of("title", Map.of("containsic", "java"));
//        Map<String, Object> clause = Map.of();
//        List<String> fields = List.of("Book_id", "Book_title");
//        DataPaging paging = new DataPaging(1, 10, Collections.singletonList(new Sort("Book.title", SortDirection.ASC)));
//        Map<String, JoinType> join = Map.of("author", JoinType.INNER);
//        Aggregates aggregates = new Aggregates(Collections.emptyList(), List.of("Book_genre"), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
//        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
//
//        DataFetchingFieldSelectionSet selectionSet = mock(DataFetchingFieldSelectionSet.class);
//        when(environment.getSelectionSet()).thenReturn(selectionSet);
//        when(selectionSet.getFields()).thenReturn(Collections.emptyList());
//
//        List<BookEntity> books = List.of(BookEntity.builder().id(UUID.randomUUID()).title("Effective Java").build());
//        Object expectedPageData = new Object();
//
//        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(BookEntity.class), eq(UUID.class), eq(filter), eq(clause), eq(fields), eq(join), eq(aggregates), eq(paging)))
//                .thenReturn(expectedPageData);
//
//        Object result = bookGraphQLController.complexBooksSearch(filter, clause, fields, paging, aggregates, environment);
//
//        assertEquals(expectedPageData, result);
//    }
//
//    @Test
//    void complexBooksSearchReturnsEmptyResults() {
//        Map<String, Object> filter = Map.of();
//        Map<String, Object> clause = Map.of();
//        List<String> fields = List.of("Book_id", "Book_title");
//        DataPaging paging = new DataPaging(1, 10, Collections.singletonList(new Sort("Book.title", SortDirection.ASC)));
//        Map<String, JoinType> join = Map.of("author", JoinType.INNER);
//        Aggregates aggregates = new Aggregates(Collections.emptyList(), List.of("Book_genre"), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
//        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
//
//        DataFetchingFieldSelectionSet selectionSet = mock(DataFetchingFieldSelectionSet.class);
//        when(environment.getSelectionSet()).thenReturn(selectionSet);
//        when(selectionSet.getFields()).thenReturn(Collections.emptyList());
//
//        PageData<BookEntity> expectedPageData = new PageData<>(List.of(), 0, 0, 10, 1, 10);
//
//        when(typedQueryBuilder.parseFilterExpression(eq(Operation.SEARCH), eq(BookEntity.class), eq(UUID.class), eq(filter), eq(clause), eq(fields), eq(join), eq(aggregates), eq(paging)))
//                .thenReturn(expectedPageData);
//
//        PageData<BookEntity> result = (PageData<BookEntity>) bookGraphQLController.complexBooksSearch(filter, clause, fields, paging, aggregates, environment);
//
//        assertEquals(expectedPageData, result);
//    }

    @Test
    void countBooksReturnsExpectedCount() {
        Map<String, Object> filter = Map.of("author", "Joshua Bloch");
        Map<String, Object> clause = Map.of();
        List<String> fields = List.of("author");

        long expectedCount = 3L;

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(BookEntity.class), eq(UUID.class), eq(filter), eq(clause), anyList(), isNull(), isNull(), isNull()))
                .thenReturn(expectedCount);

        long result = bookGraphQLController.countBooks(filter, clause, fields);

        assertEquals(expectedCount, result);
    }

    @Test
    void countBooksReturnsZeroForNoMatches() {
        Map<String, Object> filter = Map.of("author", "NonExistentAuthor");
        Map<String, Object> clause = Map.of();
        List<String> fields = List.of("author");

        long expectedCount = 0L;

        when(typedQueryBuilder.parseFilterExpression(eq(Operation.COUNT), eq(BookEntity.class), eq(UUID.class), eq(filter), eq(clause), anyList(), isNull(), isNull(), isNull()))
                .thenReturn(expectedCount);

        long result = bookGraphQLController.countBooks(filter, clause, fields);

        assertEquals(expectedCount, result);
    }
}
