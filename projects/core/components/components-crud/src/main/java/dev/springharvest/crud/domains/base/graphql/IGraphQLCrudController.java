package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.models.dtos.BaseDTO;
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
    @Operation(operationId = "search", summary = "Performs any search query on the entity.",
            description = "Use this API to retrieve entities corresponding to the passed query inside the filter and ordered according to the requested paging.",
            parameters = {
                    @Parameter(description = "The map containing the query",
                            name = "filter",
                            required = true),
                    @Parameter(description = "The paging request, determining the number of entities that should be returned, their sort orders and their sort direction as well",
                            name = "paging",
                            required = true),
            },
            responses = {@ApiResponse(responseCode = "200", description = "The queried entities ordered according to the paging.")})
    List<D> search(@RequestParam(name = "filter", required = true) Map<String, Object> filter, @RequestParam(name = "paging", required = true) DataPaging paging);

    @Operation(operationId = "search", summary = "Performs any search query on the entity.",
            description = "Use this API to retrieve entities corresponding to the passed query inside the filter.",
            parameters = {
                    @Parameter(description = "The map containing the query",
                            name = "filter",
                            required = true),
            },
            responses = {@ApiResponse(responseCode = "200", description = "The queried entities.")})
    List<D> search(@RequestParam(name = "filter", required = true) Map<String, Object> filter);
}
