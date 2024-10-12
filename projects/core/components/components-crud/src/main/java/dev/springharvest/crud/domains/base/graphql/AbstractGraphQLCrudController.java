package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.crud.domains.base.services.AbstractCrudService;
import dev.springharvest.crud.domains.base.services.AbstractSpecificationCrudService;
import dev.springharvest.expressions.helpers.JpaSpecificationBuilder;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import dev.springharvest.shared.domains.base.models.dtos.BaseDTO;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * A generic implementation of the IGraphQLCrudController interface.
 * This class provides CRUD operations for GraphQL endpoints using generics to handle different types of DTOs, entities, and primary key fields.
 *
 * @param <D> The DTO type, which extends BaseDTO<K>
 * @param <E> The entity type, which extends BaseEntity<K>
 * @param <K> The type of the primary key field, which extends Serializable
 *
 * @see IGraphQLCrudController
 * @see AbstractCrudService
 * @see BaseDTO
 * @see BaseEntity
 * @since 1.0
 */
public class AbstractGraphQLCrudController<D extends BaseDTO<K>, E extends BaseEntity<K>, K extends Serializable>
        implements IGraphQLCrudController<D, K> {

    /**
     * Mapper to convert between entities and DTOs.
     */
    protected IBaseModelMapper<D, E, K> modelMapper;

    /**
     * Service to handle CRUD operations with specifications.
     */
    protected AbstractSpecificationCrudService<E, K> crudService;

    /**
     * The class type of the entity.
     */
    protected Class<E> entityClass;

    /**
     * Constructs an AbstractGraphQLCrudController with the specified mapper, service, and entity class.
     *
     * @param modelMapper the mapper to convert between entities and DTOs
     * @param crudService the service to handle CRUD operations
     * @param entityClass the class type of the entity
     */
    protected AbstractGraphQLCrudController(IBaseModelMapper<D, E, K> modelMapper,
                                            AbstractSpecificationCrudService<E, K> crudService,
                                            Class<E> entityClass) {
        this.modelMapper = modelMapper;
        this.crudService = crudService;
        this.entityClass = entityClass;
    }

    /**
     * Searches for entities based on a filter and paging information, then converts the results to DTOs.
     *
     * @param filter the filter criteria as a map of field names to values
     * @param paging the paging information
     * @return a list of DTOs matching the filter criteria
     */
    @Override
    public List<D> search(Map<String, Object> filter, DataPaging paging) {
        var pageRequest = PageRequest.of(paging.page(), paging.size(), paging.sortDirection().name().equals("A") ? Sort.by(paging.sortOrders()).ascending() : Sort.by(paging.sortOrders()).descending());

        if (filter.isEmpty()) {
            Page<E> page = crudService.findAll(pageRequest);
            Page<D> dtos = page.hasContent() ? modelMapper.pagedEntityToPagedDto(page) : Page.empty(pageRequest);
            return dtos.getContent();
        }

        Specification<E> specification = JpaSpecificationBuilder.parseFilterExpression(filter, entityClass);
        Page<E> page = crudService.findAll(specification, pageRequest);
        Page<D> dtos = page.hasContent() ? modelMapper.pagedEntityToPagedDto(page) : Page.empty(pageRequest);
        return dtos.getContent();
    }

    /**
     * Searches for entities based on a filter without paging, then converts the results to DTOs.
     *
     * @param filter the filter criteria as a map of field names to values
     * @return a list of DTOs matching the filter criteria
     */
    @Override
    public List<D> search(Map<String, Object> filter) {
        if (filter.isEmpty()) {
            List<E> entityList = crudService.findAll();
            return !entityList.isEmpty() ? modelMapper.entityToDto(entityList) : List.of();
        }

        Specification<E> specification = JpaSpecificationBuilder.parseFilterExpression(filter, entityClass);
        List<E> entityList = crudService.findAll(specification);
        List<D> dtos = !entityList.isEmpty() ? modelMapper.entityToDto(entityList) : List.of();
        return dtos;
    }
}