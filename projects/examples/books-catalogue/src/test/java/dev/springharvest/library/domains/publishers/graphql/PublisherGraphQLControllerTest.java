package dev.springharvest.library.domains.publishers.graphql;

import dev.springharvest.library.domains.publishers.services.PublisherQueryCrudService;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.Sort;
import dev.springharvest.shared.constants.SortDirection;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.shared.constants.PageData;
import graphql.schema.DataFetchingEnvironment;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

@ExtendWith(SpringExtension.class)
@SpringBootTest
public class PublisherGraphQLControllerTest {

    @Autowired
    private PublisherGraphQLController publisherGraphQLController;

    @MockBean
    private PublisherQueryCrudService publisherQueryCrudService;

    @Test
    void searchPublishersReturnsExpectedResults() {
        Map<String, Object> filter = Map.of("name", "O'Reilly");
        Map<String, Object> clause = Map.of();
        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("name", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        List<PublisherEntity> publishers = List.of(new PublisherEntity("O'Reilly"));
        Page<PublisherEntity> publisherPage = new PageImpl<>(publishers, PageRequest.of(0, 10), publishers.size());

        Specification<PublisherEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "O'Reilly");
        Pageable pageable = PageRequest.of(0, 10, org.springframework.data.domain.Sort.by("name").ascending());

        when(publisherQueryCrudService.findAll(eq(spec), eq(pageable))).thenReturn(publisherPage);

        PageData<PublisherEntity> result = publisherGraphQLController.searchPublishers(filter, clause, paging, environment);

        assertEquals(publisherPage.getTotalElements(), result.getTotal());
        assertEquals(publisherPage.getContent(), result.getData());
    }

    @Test
    void searchPublishersReturnsEmptyResults() {
        Map<String, Object> filter = Map.of("name", "NonExistent");
        Map<String, Object> clause = Map.of();
        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("name", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        List<PublisherEntity> publishers = List.of();
        Page<PublisherEntity> publisherPage = new PageImpl<>(publishers, PageRequest.of(0, 10), publishers.size());

        Specification<PublisherEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "NonExistent");
        Pageable pageable = PageRequest.of(0, 10, org.springframework.data.domain.Sort.by("name").ascending());

        when(publisherQueryCrudService.findAll(eq(spec), eq(pageable))).thenReturn(publisherPage);

        PageData<PublisherEntity> result = publisherGraphQLController.searchPublishers(filter, clause, paging, environment);

        assertEquals(publisherPage.getTotalElements(), result.getTotal());
        assertEquals(publisherPage.getContent(), result.getData());
    }

    @Test
    void countPublishersReturnsExpectedCount() {
        Map<String, Object> filter = Map.of("type", "Tech");
        Map<String, Object> clause = Map.of();
        List<String> fields = List.of("type");

        Specification<PublisherEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("type"), "Tech");

        when(publisherQueryCrudService.findAll(eq(spec))).thenReturn(Arrays.asList(new PublisherEntity(), new PublisherEntity()));

        long count = publisherQueryCrudService.findAll(eq(spec)).size();

        long result = publisherGraphQLController.countPublishers(filter, clause, fields);

        assertEquals(result, count);
    }

    @Test
    void countPublishersReturnsZeroForNoMatches() {
        Map<String, Object> filter = Map.of("type", "NonExistentType");
        Map<String, Object> clause = Map.of();
        List<String> fields = List.of("type");
        Specification<PublisherEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("type"), "NonExistentType");

        when(publisherQueryCrudService.findAll(eq(spec))).thenReturn(Collections.emptyList());

        long count = publisherQueryCrudService.findAll(eq(spec)).size();

        long result = publisherGraphQLController.countPublishers(filter, clause, fields);

        assertEquals(result, count);
    }
}