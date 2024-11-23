package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.crud.domains.base.services.AbstractCrudService;
import dev.springharvest.crud.domains.base.services.AbstractQueryCrudService;
import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.shared.constants.Aggregates;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.PageData;
import dev.springharvest.shared.domains.base.models.dtos.BaseDTO;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import graphql.schema.DataFetchingEnvironment;
import org.springframework.beans.factory.annotation.Autowired;
import dev.springharvest.expressions.builders.TypedQueryBuilder;
import jakarta.persistence.criteria.JoinType;

import java.io.Serializable;
import java.util.*;

/**
 * A generic implementation of the IGraphQLCrudController interface.
 * This class provides CRUD operations for GraphQL endpoints using generics to handle different types of DTOs, entities, and primary key fields.
 *
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
public class AbstractGraphQLCrudController<E extends BaseEntity<K>, K extends Serializable>
        implements IGraphQLCrudController<E, K> {

    /**
     * Service to handle CRUD operations with specifications.
     */
    protected AbstractQueryCrudService<E, K> crudService;

    /**
     * The class type of the entity.
     */
    protected Class<E> entityClass;

    /**
     * The class type of the primary key field.
     */
    protected Class<K> keyClass;

    /**
     * The list of joins to be performed.
     */
    protected Map<String, JoinType> joins;

    /**
     * The TypedQueryBuilder to parse filter expressions.
     */
    @Autowired
    private TypedQueryBuilder typedQueryBuilder;

    /**
     * Constructs an AbstractGraphQLCrudController with the specified mapper, service, and entity class.
     *
     * @param crudService the service to handle CRUD operations
     * @param entityClass the class type of the entity
     */
    protected AbstractGraphQLCrudController(AbstractQueryCrudService<E, K> crudService,
                                            Class<E> entityClass, Class<K> keyClass) {
        this.crudService = crudService;
        this.entityClass = entityClass;
        this.keyClass = keyClass;
        this.joins = new HashMap<>();
    }

    @SuppressWarnings("unchecked")
    @Override
    public PageData<E> search(Map<String, Object> filter, Map<String, Object> clause, DataPaging paging, DataFetchingEnvironment environment) {
        try {
            List<String> fields = new ArrayList<>();
            environment.getSelectionSet().getFields().forEach(x -> {
                fields.add(x.getFullyQualifiedName());
                if (x.getArguments() != null && !x.getArguments().isEmpty()) {
                    x.getArguments().forEach((y, z) -> {
                        if (y.contains("join"))
                            joins.put(x.getFullyQualifiedName(), JoinType.valueOf(z.toString()));
                    });
                }
            });

            if (joins != null && !joins.isEmpty()) {
                Map<String, JoinType> updatedJoins = new LinkedHashMap<>();
                List<String> argumentNames = new ArrayList<>(joins.keySet());
                argumentNames = getFormattedFields(argumentNames);
                Iterator<String> keyIterator = joins.keySet().iterator();
                Iterator<String> argumentIterator = argumentNames.iterator();

                while (keyIterator.hasNext() && argumentIterator.hasNext()) {
                    String oldKey = keyIterator.next();
                    String newKey = argumentIterator.next();
                    updatedJoins.put(newKey, joins.get(oldKey));
                }

                joins.clear();
                joins.putAll(updatedJoins);
            }

            PageData <E> p = (PageData<E>) typedQueryBuilder.parseFilterExpression(Operation.SEARCH, entityClass, keyClass, filter, clause, getFormattedFields(fields), joins, null, paging);
            return p;
        }
        finally {
            if (joins != null && !joins.isEmpty()) {
                joins.clear();
            }
        }
    }

    @Override
    public Object search(Map<String, Object> filter, Map<String, Object> clause, List<String> fields, Aggregates aggregatesFilter, DataPaging paging) {
        List<String> formattedFields = fields != null ? getFormattedFields(fields) : null;
        return typedQueryBuilder.parseFilterExpression(Operation.SEARCH, entityClass, keyClass, filter, clause, formattedFields, null, getFormattedAggregates(aggregatesFilter, formattedFields), paging);
    }

    @Override
    public long count(Map<String, Object> filter, Map<String, Object> clause, List<String> fields) {
        return (long) typedQueryBuilder.parseFilterExpression(Operation.COUNT, entityClass, keyClass, filter, clause, getFormattedFields(fields), null, null, null);
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
            if (x.contains(".data/") || x.contains("/") || x.contains("_")) {
                if (x.contains(".data/")) {
                    String[] parts = x.split("\\.");
                    x = x.replace(parts[0] + ".", "");
                }
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
                if (!x.contains("currentPage") && !x.contains("pageSize") && !x.contains("totalPages") && !x.contains("currentPageCount") && !x.contains("total"))
                    if (!(x.split("\\.").length == 2 && x.split("\\.")[1].equals("data"))) {
                        if (!formattedFields.contains(x)) {
                            formattedFields.add(x); // This case should normally not happen
                        }
                    }
            }
        });

        return formattedFields;
    }
}
