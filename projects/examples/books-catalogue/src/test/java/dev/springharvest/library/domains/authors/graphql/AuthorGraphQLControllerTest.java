package dev.springharvest.library.domains.authors.graphql;

import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import dev.springharvest.library.domains.authors.services.AuthorQueryCrudService;
import dev.springharvest.library.domains.pet.models.entities.PetEntity;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.PageData;
import dev.springharvest.shared.constants.Sort;
import dev.springharvest.shared.constants.SortDirection;
import graphql.schema.DataFetchingEnvironment;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.jpa.domain.Specification;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

@SpringBootTest
public class AuthorGraphQLControllerTest {

    @Autowired
    private AuthorGraphQLController authorGraphQLController;

    @Autowired
    private AuthorQueryCrudService authorQueryCrudService;

    @Test
    public void searchAuthorsReturnsResults() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", "John Doe");

        Map<String, Object> clause = new HashMap<>();
        clause.put("sort", "name");

        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("name", SortDirection.ASC)));

        DataFetchingEnvironment environment = Mockito.mock(DataFetchingEnvironment.class);

        List<AuthorEntity> authors = Arrays.asList(new AuthorEntity("John Doe", new PetEntity()));
        Page<AuthorEntity> authorPage = new PageImpl<>(authors, PageRequest.of(0, 10), authors.size());

        Specification<AuthorEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "John Doe");
        Pageable pageable = PageRequest.of(0, 10);

        when(authorQueryCrudService.findAll(eq(spec), eq(pageable))).thenReturn(authorPage);

        PageData<AuthorEntity> result = authorGraphQLController.searchAuthors(filter, clause, paging, environment);

        assertEquals(1, result.getTotal());
        assertEquals("John Doe", result.getData().get(0).getName());
    }

    @Test
    public void searchAuthorsWithEmptyResults() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", "Jane Doe");

        Map<String, Object> clause = new HashMap<>();
        clause.put("sort", "name");

        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("name", SortDirection.ASC)));

        DataFetchingEnvironment environment = Mockito.mock(DataFetchingEnvironment.class);

        List<AuthorEntity> authors = Collections.emptyList();
        Page<AuthorEntity> authorPage = new PageImpl<>(authors, PageRequest.of(0, 10), authors.size());

        Specification<AuthorEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "Jane Doe");
        Pageable pageable = PageRequest.of(0, 10);

        when(authorQueryCrudService.findAll(eq(spec), eq(pageable))).thenReturn(authorPage);

        PageData<AuthorEntity> result = authorGraphQLController.searchAuthors(filter, clause, paging, environment);

        assertEquals(0, result.getTotal());
    }

    @Test
    public void searchAuthorsWithInvalidFilter() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("invalidField", "value");

        Map<String, Object> clause = new HashMap<>();
        clause.put("sort", "name");

        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("name", SortDirection.ASC)));

        DataFetchingEnvironment environment = Mockito.mock(DataFetchingEnvironment.class);

        List<AuthorEntity> authors = Collections.emptyList();
        Page<AuthorEntity> authorPage = new PageImpl<>(authors, PageRequest.of(0, 10), authors.size());

        Specification<AuthorEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("invalidField"), "value");
        Pageable pageable = PageRequest.of(0, 10);

        when(authorQueryCrudService.findAll(eq(spec), eq(pageable))).thenReturn(authorPage);

        PageData<AuthorEntity> result = authorGraphQLController.searchAuthors(filter, clause, paging, environment);

        assertEquals(0, result.getTotal());
    }

    @Test
    public void searchAuthorsWithNullPaging() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", "John Doe");

        Map<String, Object> clause = new HashMap<>();
        clause.put("sort", "name");

        DataFetchingEnvironment environment = Mockito.mock(DataFetchingEnvironment.class);

        List<AuthorEntity> authors = Arrays.asList(new AuthorEntity("John Doe", new PetEntity()));
        Page<AuthorEntity> authorPage = new PageImpl<>(authors, PageRequest.of(0, 10), authors.size());

        Specification<AuthorEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "John Doe");
        Pageable pageable = PageRequest.of(0, 10);

        when(authorQueryCrudService.findAll(eq(spec), eq(pageable))).thenReturn(authorPage);

        PageData<AuthorEntity> result = authorGraphQLController.searchAuthors(filter, clause, null, environment);

        assertEquals(1, result.getTotal());
        assertEquals("John Doe", result.getData().get(0).getName());
    }

    @Test
    public void countAuthorsReturnsCount() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", "John Doe");

        Map<String, Object> clause = new HashMap<>();
        clause.put("sort", "name");

        List<String> fields = Arrays.asList("name");

        Specification<AuthorEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "John Doe");

        when(authorQueryCrudService.findAll(eq(spec))).thenReturn(Arrays.asList(new AuthorEntity(), new AuthorEntity()));

        long count = authorQueryCrudService.findAll(eq(spec)).size();

        long result = authorGraphQLController.countAuthors(filter, clause, fields);

        assertEquals(result, count);
    }

    @Test
    public void countAuthorsWithEmptyResults() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", "Jane Doe");

        Map<String, Object> clause = new HashMap<>();
        clause.put("sort", "name");

        List<String> fields = Arrays.asList("name");

        Specification<AuthorEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "Jane Doe");

        when(authorQueryCrudService.findAll(eq(spec))).thenReturn(Arrays.asList(new AuthorEntity(), new AuthorEntity()));

        long count = authorQueryCrudService.findAll(eq(spec)).size();

        long result = authorGraphQLController.countAuthors(filter, clause, fields);

        assertEquals(result, count);
    }

    @Test
    public void countAuthorsWithInvalidFilter() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("invalidField", "value");

        Map<String, Object> clause = new HashMap<>();
        clause.put("sort", "name");

        List<String> fields = Arrays.asList("name");

        Specification<AuthorEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("invalidField"), "value");

        when(authorQueryCrudService.findAll(eq(spec))).thenReturn(Arrays.asList(new AuthorEntity(), new AuthorEntity()));

        long count = authorQueryCrudService.findAll(eq(spec)).size();

        long result = authorGraphQLController.countAuthors(filter, clause, fields);

        assertEquals(result, count);
    }

    @Test
    public void countAuthorsWithNullFields() {
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", "John Doe");

        Map<String, Object> clause = new HashMap<>();
        clause.put("sort", "name");

        Specification<AuthorEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "John Doe");

        when(authorQueryCrudService.findAll(eq(spec))).thenReturn(Arrays.asList(new AuthorEntity(), new AuthorEntity()));

        long count = authorQueryCrudService.findAll(eq(spec)).size();

        long result = authorGraphQLController.countAuthors(filter, clause, null);

        assertEquals(result, count);
    }
}