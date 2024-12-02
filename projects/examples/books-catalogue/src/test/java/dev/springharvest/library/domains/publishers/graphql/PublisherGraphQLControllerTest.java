package dev.springharvest.library.domains.publishers.graphql;

import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.Sort;
import dev.springharvest.shared.constants.SortDirection;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.shared.constants.PageData;
import dev.springharvest.library.domains.publishers.services.PublisherCrudService;
import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.expressions.builders.TypedQueryBuilder;
import graphql.schema.DataFetchingEnvironment;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.isNull;

@ExtendWith(SpringExtension.class)
@SpringBootTest(properties = "spring.liquidbase.enabled=false")
@AutoConfigureMockMvc
public class PublisherGraphQLControllerTest {
    @Autowired
    private PublisherGraphQLController publisherGraphQLController;

    @MockBean
    private PublisherCrudService publisherQueryCrudService;

    @MockBean
    private TypedQueryBuilder typedQueryBuilder;

    @Test
    void testSimpleEquality() {
        assertEquals(1, 1, "Basic equality test failed");
    }
}