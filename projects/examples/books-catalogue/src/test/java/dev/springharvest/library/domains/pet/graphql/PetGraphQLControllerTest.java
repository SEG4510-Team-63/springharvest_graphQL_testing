package dev.springharvest.library.domains.pet.graphql;

import dev.springharvest.library.domains.pet.service.PetQueryCrudService;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.Sort;
import dev.springharvest.shared.constants.SortDirection;
import dev.springharvest.library.domains.pet.models.entities.PetEntity;
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
public class PetGraphQLControllerTest {

    @Autowired
    private PetGraphQLController petGraphQLController;

    @MockBean
    private PetQueryCrudService petQueryCrudService;

    @Test
    void searchPetsReturnsExpectedResults() {
        Map<String, Object> filter = Map.of("name", "Buddy");
        Map<String, Object> clause = Map.of();
        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("name", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        List<PetEntity> pets = List.of(new PetEntity("Buddy"));
        Page<PetEntity> petPage = new PageImpl<>(pets, PageRequest.of(0, 10), pets.size());

        Specification<PetEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "Buddy");
        Pageable pageable = PageRequest.of(0, 10, org.springframework.data.domain.Sort.by("name").ascending());

        when(petQueryCrudService.findAll(eq(spec), eq(pageable))).thenReturn(petPage);

        PageData<PetEntity> result = petGraphQLController.searchPets(filter, clause, paging, environment);

        assertEquals(petPage.getTotalElements(), result.getTotal());
        assertEquals(petPage.getContent(), result.getData());
    }

    @Test
    void searchPetsReturnsEmptyResults() {
        Map<String, Object> filter = Map.of("name", "NonExistent");
        Map<String, Object> clause = Map.of();
        DataPaging paging = new DataPaging(0, 10, Collections.singletonList(new Sort("name", SortDirection.ASC)));
        DataFetchingEnvironment environment = mock(DataFetchingEnvironment.class);
        List<PetEntity> pets = List.of(new PetEntity("NonExistent"));
        Page<PetEntity> petPage = new PageImpl<>(pets, PageRequest.of(0, 10), pets.size());

        Specification<PetEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("name"), "NonExistent");
        Pageable pageable = PageRequest.of(0, 10, org.springframework.data.domain.Sort.by("name").ascending());

        when(petQueryCrudService.findAll(eq(spec), eq(pageable))).thenReturn(petPage);

        PageData<PetEntity> result = petGraphQLController.searchPets(filter, clause, paging, environment);

        assertEquals(Collections.emptyList(), result.getData());
    }

    @Test
    void countPetsReturnsExpectedCount() {
        Map<String, Object> filter = Map.of("type", "Dog");
        Map<String, Object> clause = Map.of();
        List<String> fields = List.of("type");

        Specification<PetEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("type"), "Dog");

        when(petQueryCrudService.findAll(eq(spec))).thenReturn(Arrays.asList(new PetEntity(), new PetEntity()));

        long count = petQueryCrudService.findAll(eq(spec)).size();

        long result = petGraphQLController.countPets(filter, clause, fields);

        assertEquals(result, count);
    }

    @Test
    void countPetsReturnsZeroForNoMatches() {
        Map<String, Object> filter = Map.of("type", "NonExistentType");
        Map<String, Object> clause = Map.of();
        List<String> fields = List.of("type");
        Specification<PetEntity> spec = (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("type"), "NonExistentType");

        when(petQueryCrudService.findAll(eq(spec))).thenReturn(Arrays.asList(new PetEntity(), new PetEntity()));

        long count = petQueryCrudService.findAll(eq(spec)).size();

        long result = petGraphQLController.countPets(filter, clause, fields);

        assertEquals(result, count);
    }
}
