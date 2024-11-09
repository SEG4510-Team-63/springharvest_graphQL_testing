package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.crud.domains.base.services.AbstractCrudService;
import dev.springharvest.crud.domains.base.services.AbstractQueryCrudService;
import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.expressions.builders.JpaSpecificationBuilder;
import dev.springharvest.shared.constants.Aggregates;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.PageData;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import dev.springharvest.shared.domains.base.models.dtos.BaseDTO;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import graphql.schema.DataFetchingEnvironment;
import jakarta.persistence.Tuple;
import jakarta.persistence.TupleElement;
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
 * @see TypedQueryBuilder
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
    protected AbstractQueryCrudService<E, K> crudService;

    /**
     * The class type of the entity.
     */
    protected Class<E> entityClass;

    /**
     * The list of fields to be selected.
     */
    protected List<String> Fields;

    /**
     * The TypedQueryBuilder to parse filter expressions.
     */
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

    @SuppressWarnings("unchecked")
    @Override
    public PageData<D> search(Map<String, Object> filter, Map<String, Object> clause, DataPaging paging, DataFetchingEnvironment environment) {
        environment.getSelectionSet().getFields().forEach(x -> Fields.add(x.getFullyQualifiedName()));

        List<Tuple> data = (List<Tuple>) TypedQueryBuilder.parseFilterExpression(Operation.SEARCH, entityClass, filter, clause, getFormattedFields(Fields), null, paging);

        for (Tuple result : data) {
            List<TupleElement<?>> result2 = result.getElements();
            System.out.println(result);
        }

        return new PageData<D>(List.of(), 0, 0, 0);
    }

    @Override
    public Object search(Map<String, Object> filter, Map<String, Object> clause, List<String> fields, Aggregates aggregatesFilter, DataPaging paging) {
        List<String> formattedFields = fields != null ? getFormattedFields(fields) : null;
        return TypedQueryBuilder.parseFilterExpression(Operation.SEARCH, entityClass, filter, clause, formattedFields, getFormattedAggregates(aggregatesFilter, formattedFields), paging);
    }

    @Override
    public long count(Map<String, Object> filter, Map<String, Object> clause, List<String> fields) {
        return (long) TypedQueryBuilder.parseFilterExpression(Operation.COUNT, entityClass, filter, clause, getFormattedFields(fields), null, null);
    }

    private static Aggregates getFormattedAggregates(Aggregates aggregates, List<String> fields) {
        List<String> count = aggregates.count() != null && !aggregates.count().isEmpty() ? getFormattedFields(aggregates.count()) : null;
        List<String> sum = aggregates.sum() != null && !aggregates.sum().isEmpty() ? getFormattedFields(aggregates.sum()) : null;
        List<String> avg = aggregates.avg() != null && !aggregates.avg().isEmpty() ? getFormattedFields(aggregates.avg()) : null;
        List<String> min = aggregates.min() != null && !aggregates.min().isEmpty() ? getFormattedFields(aggregates.min()) : null;
        List<String> max = aggregates.max() != null && !aggregates.max().isEmpty() ? getFormattedFields(aggregates.max()) : null;
        List<String> groupBy = aggregates.groupBy() != null && !aggregates.groupBy().isEmpty() ? getFormattedFields(aggregates.groupBy()) : null;

        //We do this because if the fields are not specified, we have to use the fields that are not in the aggregates inside the groupBy (SQL rule)
        if (fields != null && !fields.isEmpty()) {
            List<String> allFields = new ArrayList<>();
            if (count != null) allFields.addAll(count);
            if (sum != null) allFields.addAll(sum);
            if (avg != null) allFields.addAll(avg);
            if (min != null) allFields.addAll(min);
            if (max != null) allFields.addAll(max);

            for (String field : fields) {
                if (!allFields.isEmpty() && (groupBy == null || !groupBy.contains(field))) {
                    if (groupBy == null) {
                        groupBy = new ArrayList<>();
                    }
                    groupBy.add(field);
                }
            }
        }

        return new Aggregates(count, sum, avg, min, max, groupBy);
    }

    private static List<String> getFormattedFields(List<String> fields) {
        List<String> formattedFields = new ArrayList<>();

        fields.forEach(x -> {
            if (x.contains("/") || x.contains("_")) {
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
                }
                else if (x.contains("_")) {
                    String[] parts = x.split("_");
                    StringBuilder newString = new StringBuilder(parts[0]);

                    for (int i = 1; i < parts.length; i++) {
                        newString.append(".").append(parts[i]);
                    }

                    String finalString = newString.toString();
                    if (!formattedFields.contains(finalString)) {
                        formattedFields.add(finalString);
                    }
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
