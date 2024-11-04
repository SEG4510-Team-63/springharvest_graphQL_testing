package dev.springharvest.library.domains.authors.graphql;

import dev.springharvest.library.domains.authors.graphql.AuthorGraphQLController;
import dev.springharvest.library.domains.authors.models.dtos.AuthorDTO;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import dev.springharvest.library.domains.authors.services.AuthorSpecificationCrudService;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.SortDirections;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import graphql.schema.DataFetchingEnvironment;
import graphql.schema.DataFetchingFieldSelectionSet;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class AuthorGraphQLControllerTest {

    @Mock
    private AuthorSpecificationCrudService crudService;

    @Mock
    private IBaseModelMapper<AuthorDTO, AuthorEntity, UUID> modelMapper;

    @Mock
    private DataFetchingEnvironment environment;

    @Mock
    private DataFetchingFieldSelectionSet selectionSet; // Mocking the selection set

    @InjectMocks
    private AuthorGraphQLController authorGraphQLController;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);

        // Setup for DataFetchingEnvironment
        when(environment.getSelectionSet()).thenReturn(selectionSet);
        when(selectionSet.getFields()).thenReturn(Collections.emptyList()); // Or a list of mocked fields as needed
    }

    @Test
    void testSearchAuthorsWithPaging() {
        // Setup
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> operation = new HashMap<>();
        DataPaging paging = new DataPaging(0, 10, "name", SortDirections.A);

        List<AuthorDTO> mockAuthors = Arrays.asList(new AuthorDTO(), new AuthorDTO());
        when(authorGraphQLController.search(filter, operation, paging, environment)).thenReturn(mockAuthors);

        // Execute
        List<AuthorDTO> result = authorGraphQLController.searchAuthors(filter, operation, paging, environment);

        // Verify
        assertEquals(2, result.size(), "Expected 2 authors to be returned");
        verify(authorGraphQLController, times(1)).search(filter, operation, paging, environment);
    }

    @Test
    void testSearchAuthorsWithoutPaging() {
        // Setup
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> operation = new HashMap<>();

        List<AuthorDTO> mockAuthors = Arrays.asList(new AuthorDTO(), new AuthorDTO());
        when(authorGraphQLController.search(filter, operation, environment)).thenReturn(mockAuthors);

        // Execute
        List<AuthorDTO> result = authorGraphQLController.searchAuthors(filter, operation, null, environment);

        // Verify
        assertEquals(2, result.size(), "Expected 2 authors to be returned");
        verify(authorGraphQLController, times(1)).search(filter, operation, environment);
    }

    @Test
    void testCountAuthors() {
        // Setup
        Map<String, Object> filter = new HashMap<>();
        Map<String, Object> operation = new HashMap<>();
        when(authorGraphQLController.count(filter, operation)).thenReturn("10");

        // Execute
        String count = authorGraphQLController.countAuthors(filter, operation);

        // Verify
        assertEquals("10", count, "Expected count to be '10'");
        verify(authorGraphQLController, times(1)).count(filter, operation);
    }
}

