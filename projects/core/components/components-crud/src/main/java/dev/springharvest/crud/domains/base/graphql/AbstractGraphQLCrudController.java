package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.crud.domains.base.services.AbstractCrudService;
import dev.springharvest.crud.domains.base.services.AbstractQueryCrudService;
import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.expressions.builders.JpaSpecificationBuilder;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import dev.springharvest.shared.domains.base.models.dtos.BaseDTO;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import graphql.schema.DataFetchingEnvironment;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import dev.springharvest.expressions.builders.TypedQueryBuilder;

import java.io.Serializable;
import java.util.*;

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
 * @author Gilles Djawa (NeroNemesis)
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
    protected List<String> Fields;
    @Autowired
    private TypedQueryBuilder TypedQueryBuilder;

    /**
     * Constructs an AbstractGraphQLCrudController with the specified mapper, service, and entity class.
     *
     * @param modelMapper the mapper to convert between entities and DTOs
     * @param crudService the service to handle CRUD operations
     * @param entityClass the class type of the entity
     */
    protected AbstractGraphQLCrudController(IBaseModelMapper<D, E, K> modelMapper,
                                            AbstractQueryCrudService<E, K> crudService,
                                            Class<E> entityClass) {
        this.modelMapper = modelMapper;
        this.crudService = crudService;
        this.entityClass = entityClass;
        this.Fields = new ArrayList<>();
    }

    /**
     * Searches for entities based on a filter and paging information, then converts the results to DTOs.
     *
     * @param filter the filter criteria as a map of field names to values
     * @param paging the paging information
     * @return a list of DTOs matching the filter criteria
     */
    @Override
    public List<D> search(Map<String, Object> filter, Map<String, Object> clause, DataPaging paging, DataFetchingEnvironment environment) {
        var pageRequest = PageRequest.of(paging.page(), paging.size(), paging.sortDirection().name().equals("A") ? Sort.by(paging.sortOrders()).ascending() : Sort.by(paging.sortOrders()).descending());

//        if (filter.isEmpty()) {
//            if (operation.isEmpty())
//            {
//                Page<E> page = crudService.findAll(pageRequest);
//                Page<D> dtos = page.hasContent() ? modelMapper.pagedEntityToPagedDto(page) : Page.empty(pageRequest);
//                return dtos.getContent();
//            }
//        }

        environment.getSelectionSet().getFields().forEach(x -> Fields.add(x.getFullyQualifiedName()));
//        System.out.println("Search 1");
//        System.out.println(Fields);
//
//        System.out.println("-----------------------------------");
        //System.out.println(getFormattedFields(Fields));
        List<String> fields2 = Fields;
        Specification<E> specification = JpaSpecificationBuilder.parseFilterExpression(filter, entityClass, getFormattedFields(Fields));
        List<Object> things = TypedQueryBuilder.parseFilterExpression(filter, clause, entityClass, getFormattedFields(fields2), Operation.SEARCH);
        System.out.println("things : " + things);
        System.out.println("things length : " + things.size());

        for (Object result : things) {
            Object [] result2 = (Object[]) result;
            for (Object o : result2) {
                System.out.println("i : " + ((String) o.toString()));
            }
        }
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
    public List<D> search(Map<String, Object> filter, Map<String, Object> clause, DataFetchingEnvironment environment) {
        if (filter.isEmpty()) {
            List<E> entityList = crudService.findAll();
            return !entityList.isEmpty() ? modelMapper.entityToDto(entityList) : List.of();
        }

        environment.getSelectionSet().getFields().forEach(x -> Fields.add(x.getName()));
        System.out.println("Search 2");
        System.out.println(Fields);
        Specification<E> specification = JpaSpecificationBuilder.parseFilterExpression(filter, entityClass, getFormattedFields(Fields));
        List<E> entityList = crudService.findAll(specification);
        List<D> dtos = !entityList.isEmpty() ? modelMapper.entityToDto(entityList) : List.of();
        return dtos;
    }

    @Override
    public String count(Map<String, Object> filter, Map<String, Object> clause, List<String> fields) {
        List<Object> things = TypedQueryBuilder.parseFilterExpression(filter, clause, entityClass, fields, Operation.COUNT);
        System.out.println("things : " + things);
        System.out.println("things length : " + things.size());

        for (Object result : things) {
            if (result instanceof Long)
                return result.toString();
        }

        return "-1";
    }

    private static List<String> getFormattedFields(List<String> fields) {
        List<String> formattedFields = new ArrayList<>();

        fields.forEach(x -> {
            if (x.contains("/")) {
                String[] parts = x.split("/");
                StringBuilder newString = new StringBuilder(parts[0]);

                for (int i = 1; i < parts.length; i++) {
                    newString.append(".").append(parts[i].split("\\.")[1]);
                }

                String finalString = newString.toString();
                if (!formattedFields.contains(finalString)) {
                    formattedFields.add(finalString);
                }
            } else {
                if (!x.contains("_results__count___"))
                    if (!formattedFields.contains(x)) {
                        formattedFields.add(x);
                    }
            }
        });

        return formattedFields;
    }
}
