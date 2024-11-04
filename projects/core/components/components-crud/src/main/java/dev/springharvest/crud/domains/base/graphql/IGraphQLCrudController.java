package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.models.dtos.BaseDTO;
import graphql.schema.DataFetchingEnvironment;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.web.bind.annotation.RequestParam;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * This interface is used to define the contract for a base GraphQL controller. The @Operation annotation is used to define the OpenAPI specification for the
 * controller.
 *
 * @param <D> The DTO object for a domain
 * @param <K> The type of the id (primary key) field pertaining to the entity relating to the DTO
 */
public interface IGraphQLCrudController<D extends BaseDTO<K>, K extends Serializable> {
    @Operation(operationId = "search", summary = "Performs almost any type of search query on the entity.",
            description = "Use this API to retrieve entities corresponding to the passed query inside the filter and ordered according to the requested paging.",
            parameters = {
                    @Parameter(description = "The map containing the query",
                            name = "filter",
                            required = true),
                    @Parameter(description = "The map containing the type of clause to be performed on the query",
                            name = "clause",
                            required = true),
                    @Parameter(description = "The paging request, determining the number of entities that should be returned, their sort orders and their sort direction as well",
                            name = "paging",
                            required = true),
                    @Parameter(description = "The GraphQL data fetching environment that allows us to retrieve what fields where selected to be displayed by the user",
                            name = "environment",
                            required = true),
            },
            responses = {@ApiResponse(responseCode = "200", description = "The queried entities ordered according to the paging or their count.")})
    List<D> search(@RequestParam(name = "filter") Map<String, Object> filter, @RequestParam(name = "clause") Map<String, Object> clause, @RequestParam(name = "paging") DataPaging paging, @RequestParam(name = "environment") DataFetchingEnvironment environment);

    @Operation(operationId = "search", summary = "Performs almost any type of search query on the entity.",
            description = "Use this API to retrieve entities corresponding to the passed query inside the filter.",
            parameters = {
                    @Parameter(description = "The map containing the query",
                            name = "filter",
                            required = true),
                    @Parameter(description = "The map containing the type of clause to be performed on the query",
                            name = "clause",
                            required = true),
                    @Parameter(description = "The GraphQL data fetching environment that allows us to retrieve what fields where selected to be displayed by the user",
                            name = "environment",
                            required = true),
            },
            responses = {@ApiResponse(responseCode = "200", description = "The queried entities.")})
    List<D> search(@RequestParam(name = "filter") Map<String, Object> filter, @RequestParam(name = "clause") Map<String, Object> clause, @RequestParam(name = "environment") DataFetchingEnvironment environment);

    @Operation(operationId = "count", summary = "Performs count operations on an entity.",
            description = "Use this API to retrieve entities count corresponding to the passed query inside the filter.",
            parameters = {
                    @Parameter(description = "The map containing the query",
                            name = "filter",
                            required = true),
                    @Parameter(description = "The map containing the type of clause to be performed on the query",
                            name = "clause",
                            required = true),
                    @Parameter(description = "The fields to applay the distinct clause on",
                            name = "fields",
                            required = true)
            },
            responses = {@ApiResponse(responseCode = "200", description = "The queried entities count.")})
    String count(@RequestParam(name = "filter") Map<String, Object> filter, @RequestParam(name = "clause") Map<String, Object> clause, @RequestParam(name = "fields") List<String> fields);
}
