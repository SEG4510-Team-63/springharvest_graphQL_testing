package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.shared.constants.Aggregates;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.PageData;
import dev.springharvest.shared.domains.base.models.dtos.BaseDTO;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import graphql.schema.DataFetchingEnvironment;
import lombok.Setter;
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
 * @see BaseDTO
 * @see BaseEntity
 * @see TypedQueryBuilder
 * @author Gilles Djawa (NeroNemesis)
 * @since 1.0
 */
public class AbstractGraphQLCrudController<E extends BaseEntity<K>, K extends Serializable>
        implements IGraphQLCrudController<E, K> {

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
    @Setter
    @Autowired
    private TypedQueryBuilder typedQueryBuilder;

    /**
     * Constructs an AbstractGraphQLCrudController with the specified mapper, service, and entity class.
     *
     * @param entityClass the class type of the entity
     */
    protected AbstractGraphQLCrudController(Class<E> entityClass, Class<K> keyClass) {
        this.entityClass = entityClass;
        this.keyClass = keyClass;
        this.joins = new HashMap<>();
    }

    @SuppressWarnings("unchecked")
    @Override
    public PageData<E> search(Map<String, Object> filter, Map<String, Object> clause, DataPaging paging, DataFetchingEnvironment environment) {
        try {
            List<String> fields = extractFieldsFromEnvironment(environment);
            processJoins();

            return (PageData<E>) typedQueryBuilder.parseFilterExpression(Operation.SEARCH, entityClass, keyClass, filter, clause, getFormattedFields(fields), joins, null, paging);
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
        Aggregates aggregates = getFormattedAggregates(aggregatesFilter, formattedFields);
        return typedQueryBuilder.parseFilterExpression(Operation.SEARCH, entityClass, keyClass, filter, clause, formattedFields, null, aggregates, paging);
    }

    @Override
    public long count(Map<String, Object> filter, Map<String, Object> clause, List<String> fields) {
        return (long) typedQueryBuilder.parseFilterExpression(Operation.COUNT, entityClass, keyClass, filter, clause, getFormattedFields(fields), null, null, null);
    }

    /**
     * Formats the provided aggregates and fields.
     * <p>
     * This method formats the aggregates by converting their fields to a standardized format.
     * If the fields are not specified, it ensures that the fields not in the aggregates are included in the groupBy list.
     * Expects the fields in the aggregates list to be in the format "field1.field2.field3" or "field1_field2_field3" and the fields list to be in the format "field1.field_x", "field2.field_z", "field3.field_y".
     *
     * @param aggregates The aggregates to be formatted.
     * @param fields The list of fields to be included in the groupBy list if not already present.
     * @return A new Aggregates object with formatted fields, or null if the input aggregates are null.
     */
    static Aggregates getFormattedAggregates(Aggregates aggregates, List<String> fields) {
        if (aggregates == null) {
            return null;
        }
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

    /**
     * Formats the provided list of fields.
     * <p>
     * This method processes each field in the provided list and converts it to a standardized format.
     * It handles fields containing ".data/", "/", or "_" by splitting and reformatting them.
     * "/" come from the DataFetchingEnvironment, "_" come from custom fields in the GraphQL query.
     * Fields related to pagination (e.g., "currentPage", "pageSize") are handled separately.
     *
     * @param fields The list of fields to be formatted.
     * @return A list of formatted fields.
     */
    static List<String> getFormattedFields(List<String> fields) {
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
                if (!x.contains("currentPage") && !x.contains("pageSize") && !x.contains("totalPages") && !x.contains("currentPageCount") && !x.contains("total")){
                    if (!(x.split("\\.").length == 2 && x.split("\\.")[1].equals("data"))) {
                        if (!formattedFields.contains(x)) {
                            formattedFields.add(x); // This case should normally not happen
                        }
                    }
                }
                else
                {
                    if (x.contains("currentPage")) {
                        String [] parts = x.split("\\.");
                        if (parts.length == 2 && parts[1].equals("currentPage"))
                            formattedFields.add("currentPage");
                    }
                    if (x.contains("pageSize"))
                        formattedFields.add("pageSize");
                    if (x.contains("totalPages"))
                        formattedFields.add("totalPages");
                    if (x.contains("currentPageCount"))
                        formattedFields.add("currentPageCount");
                    if (x.contains("total")) {
                        String [] parts = x.split("\\.");
                        if (parts.length == 2 && parts[1].equals("total"))
                            formattedFields.add("total");
                    }
                }
            }
        });

        return formattedFields;
    }

    /**
     * Extracts the list of fields from the DataFetchingEnvironment.
     * <p>
     * This method retrieves the fields from the provided DataFetchingEnvironment and returns them as a list of strings.
     * It also processes any join arguments and adds them to the joins map.
     *
     * @param environment The DataFetchingEnvironment containing the selection set.
     * @return A list of field names extracted from the environment, or null if the environment or its selection set is null.
     */
    private List<String> extractFieldsFromEnvironment(DataFetchingEnvironment environment) {
        if (environment == null || environment.getSelectionSet() == null || environment.getSelectionSet().getFields() == null) {
            return null;
        }
        if ( environment.getSelectionSet().getFields().isEmpty() ) {
            return new ArrayList<>();
        }
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
        return fields;
    }

    /**
     * Processes the joins by formatting the join keys.
     * <p>
     * This method updates the `joins` map by formatting the keys using the `getFormattedFields` method.
     * It creates a new map with the formatted keys and the corresponding join types, then replaces the original `joins` map with the updated one.
     */
    void processJoins() {
        if (joins == null || joins.isEmpty()) {
            return;
        }

        Map<String, JoinType> updatedJoins = new LinkedHashMap<>();
        List<String> formattedFields = getFormattedFields(new ArrayList<>(joins.keySet()));
        Iterator<String> keyIterator = joins.keySet().iterator();
        Iterator<String> formattedFieldIterator = formattedFields.iterator();

        while (keyIterator.hasNext() && formattedFieldIterator.hasNext()) {
            String oldKey = keyIterator.next();
            String newKey = formattedFieldIterator.next();
            updatedJoins.put(newKey, joins.get(oldKey));
        }

        joins.clear();
        joins.putAll(updatedJoins);
    }
}
