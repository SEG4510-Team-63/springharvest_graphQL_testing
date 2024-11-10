package dev.springharvest.expressions.builders;

import dev.springharvest.expressions.helpers.Operation;
import dev.springharvest.expressions.helpers.Operator;
import dev.springharvest.expressions.mappers.GenericEntityMapper;
import dev.springharvest.shared.constants.Aggregates;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.Sort;
import dev.springharvest.shared.constants.SortDirection;
import jakarta.persistence.*;
import jakarta.persistence.criteria.*;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.lang.reflect.Field;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * This class is responsible for helping build Typed queries from a filter map.
 * It provides methods to parse filter expressions and create typed queries for querying entities.
 *
 * @author NeroNemesis
 *
 * @since 1.0
 * @see Operation
 * @see Operator
 * @see Aggregates
 * @see DataPaging
 * @see Sort
 * @see SortDirection
 * @see EntityManagerFactory
 * @see EntityManager
 */
@Component
public class TypedQueryBuilder {

    /**
     * The entity manager factory.
     * This is used to create an entity manager.
     * The entity manager is used to create a criteria builder.
     * The criteria builder is used to create a criteria query.
     * The criteria query is used to create a root.
     * The root is used to create a path.
     * The path is used to create a predicate.
     * The predicate is used to create a typed query.
     * The typed query is used to execute the query.
     * The query is used to get the result list.
     */
    @Autowired
    private EntityManagerFactory entityManagerFactory;

    /**
     * Parses a filter expression and creates a typed query for querying entities.
     *
     * @param operation The operation to be performed (e.g., SEARCH, COUNT).
     * @param rootClass The class of the root entity.
     * @param filterMap A map containing filter criteria.
     * @param clauseMap A map containing additional clauses.
     * @param fields A list of fields to be included in the query.
     * @param aggregatesFilter Aggregates to be applied to the query.
     * @param paging Paging information for the query.
     * @param <T> The type of the root entity.
     * @return The result of the query, which can be a list of entities or a count.
     * @throws RuntimeException If a field specified in the fields list is not found in the root class.
     */
    public <T> Object parseFilterExpression(Operation operation,
                                            Class<T> rootClass,
                                            Map<String, Object> filterMap,
                                            Map<String, Object> clauseMap,
                                            List<String> fields,
                                            Aggregates aggregatesFilter,
                                            DataPaging paging) {

        EntityManager entityManager = entityManagerFactory.createEntityManager();

        try {
            try {
                fields = CleanFields(rootClass, fields);
            } catch (NoSuchFieldException e) {
                throw new RuntimeException("Field not found.\n For each field make sure the field's name exists in the class corresponding to your schema definition and follows this format 'Schema type name' + '.' + 'fieldName.\n e.g: 'Book.title', 'Book.author.pet.name'.");
            }
            CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
            CriteriaQuery<Tuple> criteriaQuery = criteriaBuilder.createQuery(Tuple.class);
            Root<T> root = criteriaQuery.from(rootClass);

            if (filterMap == null || filterMap.isEmpty()) {
                return applyOperations(criteriaBuilder, criteriaQuery, entityManager, clauseMap, root, fields, operation, null, rootClass, null, aggregatesFilter, paging);
            }

            String rootOperator = determineRootOperator(filterMap);
            Predicate predicate = createTypedQuery(filterMap, criteriaBuilder, rootClass, root, "", rootOperator, fields, criteriaQuery);
            criteriaQuery.where(predicate);

            return applyOperations(criteriaBuilder, criteriaQuery, entityManager, clauseMap, root, fields, operation, filterMap, rootClass, rootOperator, aggregatesFilter, paging);

        } finally {
            if (entityManager != null && entityManager.isOpen()) {
                entityManager.close();
            }
        }
    }

    /**
     * Applies operations to the criteria query based on the specified operation.
     *
     * @param criteriaBuilder The criteria builder used to construct the query.
     * @param criteriaQuery The criteria query to which operations will be applied.
     * @param entityManager The entity manager used to execute the query.
     * @param operationMap A map containing additional operation parameters.
     * @param root The root entity in the query.
     * @param fields A list of fields to be included in the query.
     * @param operation The operation to be performed (e.g., SEARCH, COUNT).
     * @param filterMap A map containing filter criteria.
     * @param rootClass The class of the root entity.
     * @param rootOperator The root operator for combining predicates.
     * @param aggregates Aggregates to be applied to the query.
     * @param paging Paging information for the query.
     * @param <T> The type of the root entity.
     * @return The result of the query, which can be a list of entities or a count.
     * @throws IllegalArgumentException If an unsupported operation is specified.
     */
    private <T> Object applyOperations(CriteriaBuilder criteriaBuilder, CriteriaQuery<Tuple> criteriaQuery, EntityManager entityManager, Map<String, Object> operationMap, Root<T> root, List<String> fields, Operation operation, Map<String, Object> filterMap, Class<T> rootClass, String rootOperator, Aggregates aggregates, DataPaging paging)
    {
        boolean distinct = operationMap != null && Boolean.TRUE.equals(operationMap.get("distinct"));
        long total;

        switch (operation) {
            case SEARCH:
                applyFieldProjection(criteriaBuilder, criteriaQuery, root, fields, aggregates, paging);
                if (distinct) {
                    criteriaQuery.distinct(true);
                }
                TypedQuery<Tuple> query = entityManager.createQuery(criteriaQuery);
                total = handleCountOperation(criteriaBuilder, criteriaQuery, entityManager, filterMap, rootClass, fields, rootOperator, distinct, root, true, aggregates);
                query.setFirstResult((paging.page() - 1) * paging.size());
                query.setMaxResults(paging.size());
                int totalPages = (int) Math.ceil((double) total / paging.size());
                //Need the mapper here
                List<Tuple> resultsList = query.getResultList();
                int currentPageCount = resultsList.size();
                List<Map<String, Object>> dataMap = mapTuplesToMap(resultsList);
                Map<String, Object> pagingMap = new HashMap<>();
                Map<String, Object> subPagingMap = new HashMap<>();
                subPagingMap.put("page", paging.page());
                subPagingMap.put("size", paging.size());
                subPagingMap.put("currentPageCount", currentPageCount);
                subPagingMap.put("totalCount", total);
                subPagingMap.put("totalPages", totalPages);

                pagingMap.put("paging", subPagingMap);
                dataMap.add(pagingMap);
                return dataMap;

            case COUNT:
                return handleCountOperation(criteriaBuilder, criteriaQuery, entityManager, filterMap, rootClass, fields, rootOperator, distinct, root, false, null);

            default:
                throw new IllegalArgumentException("Unsupported operation");
        }
    }

    /**
     * Handles the count operation for a criteria query.
     *
     * @param <T> The type of the root entity.
     * @param criteriaBuilder The criteria builder used to construct the query.
     * @param criteriaQuery The criteria query to which operations will be applied.
     * @param entityManager The entity manager used to execute the query.
     * @param filterMap A map containing filter criteria.
     * @param rootClass The class of the root entity.
     * @param fields A list of fields to be included in the query.
     * @param rootOperator The root operator for combining predicates.
     * @param distinct Whether the count should be distinct.
     * @param root The root entity in the query.
     * @param isSubQuery Whether the query is a subquery.
     * @param aggregates Aggregates to be applied to the query.
     * @return The result of the count operation.
     */
    private <T> long handleCountOperation(CriteriaBuilder criteriaBuilder, CriteriaQuery<Tuple> criteriaQuery, EntityManager entityManager, Map<String, Object> filterMap, Class<T> rootClass, List<String> fields, String rootOperator, boolean distinct, Root<T> root, boolean isSubQuery, Aggregates aggregates) {
        CriteriaQuery<Long> countQuery = criteriaBuilder.createQuery(Long.class);
        Root<?> countRoot = countQuery.from(root.getJavaType());

        Predicate filterPredicate = filterMap != null ? createTypedQuery(filterMap, criteriaBuilder, rootClass, (Root) countRoot, "", rootOperator, fields, criteriaQuery) : null;
        if (filterPredicate != null && !isSubQuery) {
            countQuery.where(filterPredicate);
        }

        if (isSubQuery)
        {
            List<Selection<?>> selections = new ArrayList<>();
            Subquery<Tuple> subquery = countQuery.subquery(Tuple.class);
            Root<?> subRoot = subquery.from(countRoot.getJavaType());
            List<Path<?>> groupByPaths = new ArrayList<>();
            for (String field : fields) {
                Object [] pathAndAlias = getPathFromField(subRoot, field);
                Path<?> path = (Path<?>) pathAndAlias[0];
                selections.add(path);
            }
            if (aggregates != null) {
                System.out.println("Aggregates in sub: " + aggregates);
                addAggregateSelections(criteriaBuilder, subRoot, aggregates, selections);
                addGroupByPaths(subRoot, aggregates, groupByPaths);
                System.out.println("Group by paths: " + groupByPaths);
                if (!groupByPaths.isEmpty()) {
                    subquery.groupBy(groupByPaths.toArray(new Path[0]));
                }
            }
            subquery.select((Expression<Tuple>) criteriaBuilder.tuple(selections.toArray(new Selection[0])));
            subquery.where(filterPredicate);
            countQuery.select(criteriaBuilder.count(subRoot)).where(criteriaBuilder.exists(subquery));
            countQuery.select(criteriaBuilder.count(countRoot));
            return entityManager.createQuery(countQuery).getSingleResult();
        }

        /* PLEASE READ
         * The following code is a workaround for the limitation of the Count and CountDistinct methods in CriteriaBuilder.
         * The Count and CountDistinct methods only support selecting 1 field.
         * Counting multiple fields is supported through Concatenation in Postgres and MySQL but not in T-SQL. Not sure about H2.
         * So I think it is preferable to avoid using Concatenation for now.
         * The workaround is to select the fields to be counted and then add the count to the result list.
         * The count is then extracted from the result list.
         */
        if (distinct) {
            if (fields == null || fields.isEmpty()) {
                countQuery.select(criteriaBuilder.countDistinct(countRoot));
                return entityManager.createQuery(countQuery).getSingleResult();
            } else {
                countQuery.select(criteriaBuilder.countDistinct((Path<?>) getPathFromField(countRoot, fields.getFirst())[0]));
                return entityManager.createQuery(countQuery).getSingleResult();
            }
        } else {
            if (fields == null || fields.isEmpty())
                countQuery.select(criteriaBuilder.count(countRoot));
            else
                countQuery.select(criteriaBuilder.count((Path<?>) getPathFromField(countRoot, fields.getFirst())[0]));
            return entityManager.createQuery(countQuery).getSingleResult();
        }
    }

    /**
     * Creates a typed query based on the provided filter map.
     *
     * @param <T> The type of the root entity.
     * @param filterMap A map containing filter criteria.
     * @param builder The criteria builder used to construct the query.
     * @param rootClass The class of the root entity.
     * @param root The root entity in the query.
     * @param parentPath The parent path for nested fields.
     * @param rootOperator The root operator for combining predicates.
     * @param fields A list of fields to be included in the query.
     * @param query The criteria query to which predicates will be applied.
     * @return A combined predicate representing the filter criteria.
     */
    @SuppressWarnings("unchecked") // Made sure these casts are safe
    private static <T> Predicate createTypedQuery(Map<String, Object> filterMap, CriteriaBuilder builder, Class<T> rootClass, Root<T> root, String parentPath, String rootOperator, List<String> fields, CriteriaQuery<?> query) {
        List<Predicate> predicates = new ArrayList<>();

        for (Map.Entry<String, Object> entry : filterMap.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (isLogicalOperator(key)) {
                if (Operator.getOperator(key).getKind() != Operator.Kind.UNARY) {
                    predicates.add(createLogicalOperatorTypedQuery(key, (List<Map<String, Object>>) value, builder, rootClass, root, parentPath, fields, query));
                } else {
                    predicates.add(createUnaryOperatorTypedQuery((Map<String, Object>) value, builder, rootClass, root, parentPath, fields, query));
                }
            } else {
                try {
                    if (isComplexField(rootClass, key)) {
                        String newPath = parentPath.isEmpty() ? key : parentPath + "." + key;
                        Class<?> nestedClass = getFieldType(rootClass, key);
                        String subOperator = determineRootOperator((Map<String, Object>) value);
                        predicates.add(createTypedQuery((Map<String, Object>) value, builder, (Class) nestedClass, root, newPath, subOperator, fields, query));
                    } else {
                        predicates.add(createSimpleFieldTypedQuery(key, value, parentPath, rootClass, builder, root, fields));
                    }
                } catch (NoSuchFieldException e) {
                    throw new RuntimeException(e);
                }
            }
        }

        return combinePredicates(predicates, builder, rootOperator, query, root);
    }

    /**
     * Creates a typed query for logical operators (AND, OR, etc.) based on the provided filter map.
     *
     * @param <T> The type of the root entity.
     * @param operator The logical operator to be applied (e.g., AND, OR).
     * @param value A list of filter maps representing the sub-filters.
     * @param builder The criteria builder used to construct the query.
     * @param rootClass The class of the root entity.
     * @param root The root entity in the query.
     * @param parentPath The parent path for nested fields.
     * @param fields A list of fields to be included in the query.
     * @param query The criteria query to which predicates will be applied.
     * @return A combined predicate representing the logical operation.
     */
    private static <T> Predicate createLogicalOperatorTypedQuery(String operator, List<Map<String, Object>> value, CriteriaBuilder builder, Class<T> rootClass, Root<T> root, String parentPath, List<String> fields, CriteriaQuery<?> query) {
        List<Predicate> subPredicates = new ArrayList<>();

        for (Map<String, Object> subFilter : value) {
            String subOperator = determineRootOperator(subFilter);
            subPredicates.add(createTypedQuery(subFilter, builder, rootClass, root, parentPath, subOperator, fields, query));
        }

        return combinePredicates(subPredicates, builder, operator, query, root);
    }

    /**
     * Creates a typed query for unary operators (NOT) based on the provided filter map.
     *
     * @param <T> The type of the root entity.
     * @param value A map containing filter criteria.
     * @param builder The criteria builder used to construct the query.
     * @param rootClass The class of the root entity.
     * @param root The root entity in the query.
     * @param parentPath The parent path for nested fields.
     * @param fields A list of fields to be included in the query.
     * @param query The criteria query to which predicates will be applied.
     * @return A predicate representing the NOT operation.
     */
    private static <T> Predicate createUnaryOperatorTypedQuery(Map<String, Object> value, CriteriaBuilder builder, Class<T> rootClass, Root<T> root, String parentPath, List<String> fields, CriteriaQuery<?> query) {
        Predicate predicate = createTypedQuery(value, builder, rootClass, root, parentPath, determineRootOperator(value), fields, query);
        return builder.not(predicate);
    }

    /**
     * Creates a typed query for a simple field based on the provided key and value.
     *
     * @param <T> The type of the root entity.
     * @param key The key representing the field name.
     * @param value The value to be matched against the field.
     * @param parentPath The parent path for nested fields.
     * @param rootClass The class of the root entity.
     * @param builder The criteria builder used to construct the query.
     * @param root The root entity in the query.
     * @param fields A list of fields to be included in the query.
     * @return A predicate representing the filter criteria for the simple field.
     */
    private static <T> Predicate createSimpleFieldTypedQuery(String key, Object value, String parentPath, Class<T> rootClass, CriteriaBuilder builder, Root<T> root, List<String> fields) {
        Path<?> path = getPath(root, parentPath, key, rootClass);
        return createPredicate(builder, path, value, getFieldType(rootClass, key));
    }

    /**
     * Applies field projection to the criteria query based on the specified fields and aggregates.
     *
     * @param builder The criteria builder used to construct the query.
     * @param query The criteria query to which projections will be applied.
     * @param root The root entity in the query.
     * @param fields A list of fields to be included in the projection.
     * @param aggregates Aggregates to be applied to the query.
     * @param paging Paging information for the query.
     */
    private static void applyFieldProjection(CriteriaBuilder builder, CriteriaQuery<?> query, Root<?> root, List<String> fields, Aggregates aggregates, DataPaging paging) {
        List<Selection<?>> selections = new ArrayList<>();
        List<Path<?>> groupByPaths = new ArrayList<>();

        for (String field : fields) {
            Object[] pathAndAlias = getPathFromField(root, field);
            Path<?> path = (Path<?>) pathAndAlias[0];
            String alias = (String) pathAndAlias[1];
            selections.add(path.alias(alias));
        }

        if (aggregates != null) {
            addAggregateSelections(builder, root, aggregates, selections);
            addGroupByPaths(root, aggregates, groupByPaths);
            if (!groupByPaths.isEmpty()) {
                query.groupBy(groupByPaths.toArray(new Path[0]));
            }
        }

        query.multiselect(selections);
        if (!groupByPaths.isEmpty()) {
            query.groupBy(groupByPaths.toArray(new Path[0]));
        }

        applySorting(builder, query, root, paging);
    }

    /**
     * Applies sorting to the criteria query based on the specified paging information.
     *
     * @param builder The criteria builder used to construct the query.
     * @param query The criteria query to which sorting will be applied.
     * @param root The root entity in the query.
     * @param paging Paging information for the query, including sort orders.
     */
    private  static void applySorting(CriteriaBuilder builder, CriteriaQuery<?> query, Root<?> root, DataPaging paging) {
        if (paging.sortOrders() != null && !paging.sortOrders().isEmpty()) {
            List<Order> orders = new ArrayList<>();
            for (Sort sortOrder : paging.sortOrders()) {
                Object[] pathAndAlias = getPathFromField(root, sortOrder.field());
                Path<?> path = (Path<?>) pathAndAlias[0];
                orders.add(sortOrder.sortDirection() == SortDirection.ASC ? builder.asc(path) : builder.desc(path));
            }
            query.orderBy(orders);
        }
    }

    /**
     * Adds aggregate selections to the criteria query based on the specified aggregates.
     *
     * @param builder The criteria builder used to construct the query.
     * @param root The root entity in the query.
     * @param aggregates Aggregates to be applied to the query.
     * @param selections A list of selections to which aggregate selections will be added.
     */
    private static void addAggregateSelections(CriteriaBuilder builder, Root<?> root, Aggregates aggregates, List<Selection<?>> selections) {
        addAggregateSelection(builder, root, aggregates.avg(), selections, "avg_", builder::avg);
        addAggregateSelection(builder, root, aggregates.max(), selections, "max_", builder::max);
        addAggregateSelection(builder, root, aggregates.min(), selections, "min_", builder::min);
        addAggregateSelection(builder, root, aggregates.sum(), selections, "sum_", builder::sum);
        addCountSelections(builder, root, aggregates.count(), selections);
    }

    /**
     * Adds aggregate selections to the criteria query based on the specified aggregates.
     *
     * @param builder The criteria builder used to construct the query.
     * @param root The root entity in the query.
     * @param aggregateFunction The aggregate function to be applied.
     * @param selections A list of selections to which aggregate selections will be added.
     */
    private static void addAggregateSelection(CriteriaBuilder builder, Root<?> root, List<String> fields, List<Selection<?>> selections, String prefix, Function<Expression<Number>, Expression<?>> aggregateFunction) {
        if (fields != null) {
            for (String field : fields) {
                Object[] pathAndAlias = getPathFromField(root, field);
                Path<?> path = (Path<?>) pathAndAlias[0];
                String alias = (String) pathAndAlias[1];
                selections.add(aggregateFunction.apply((Expression<Number>) path).alias(prefix + alias));
            }
        }
    }

    /**
     * Adds count selections to the criteria query based on the specified fields.
     *
     * @param builder The criteria builder used to construct the query.
     * @param root The root entity in the query.
     * @param fields A list of fields to be included in the count selection.
     * @param selections A list of selections to which count selections will be added.
     */
    private static void addCountSelections(CriteriaBuilder builder, Root<?> root, List<String> fields, List<Selection<?>> selections) {
        if (fields != null) {
            for (String field : fields) {
                Object[] pathAndAlias = getPathFromField(root, field);
                Path<?> path = (Path<?>) pathAndAlias[0];
                String alias = (String) pathAndAlias[1];
                selections.add(builder.count(path).alias("count_" + alias));
            }
        }
    }

    /**
     * Adds group by paths to the criteria query based on the specified aggregates.
     *
     * @param root The root entity in the query.
     * @param aggregates Aggregates containing the group by fields.
     * @param groupByPaths A list of paths to which group by paths will be added.
     */
    private static void addGroupByPaths(Root<?> root, Aggregates aggregates, List<Path<?>> groupByPaths) {
        if (aggregates != null && aggregates.groupBy() != null) {
            for (String field : aggregates.groupBy()) {
                Object[] pathAndAlias = getPathFromField(root, field);
                Path<?> path = (Path<?>) pathAndAlias[0];
                groupByPaths.add(path);
            }
        }
    }

    /**
     * Retrieves the path and alias from the specified field in the root entity.
     *
     * @param root The root entity in the query.
     * @param field The field for which the path and alias are to be retrieved.
     * @return An array containing the path and alias. The first element is the path, and the second element is the alias.
     */
    private static Object[] getPathFromField(Root<?> root, String field) {
        String[] pathParts = field.split("\\.");
        Path<?> path = root;
        StringBuilder alias = new StringBuilder();

        for (int i = 1; i < pathParts.length; i++) {
            path = path.get(pathParts[i]);
            alias.append((!alias.isEmpty()) ? "." + pathParts[i] : pathParts[i]);
        }

        return new Object [] { path, alias.toString() };
    }

    /**
     * Finds the primary key field of the specified entity class.
     *
     * @param entityClass The class of the entity for which the primary key field is to be found.
     * @return An Optional containing the name of the primary key field, or an empty Optional if no primary key field is found.
     */
    public static Optional<String> findPrimaryKeyField(Class<?> entityClass) {
        // Inspect superclass for @Id annotated field
        Class<?> currentClass = entityClass;
        while (currentClass != null) {
            for (Field field : currentClass.getDeclaredFields()) {
                if (field.isAnnotationPresent(Id.class)) {
                    // Check for AttributeOverride in subclass
                    AttributeOverride[] overrides = entityClass.getAnnotationsByType(AttributeOverride.class);
                    for (AttributeOverride override : overrides) {
                        if (override.name().equals(field.getName())) {
                            return Optional.of(override.column().name());
                        }
                    }
                    return Optional.of(field.getName());
                }
            }
            currentClass = currentClass.getSuperclass();
        }
        return Optional.empty();
    }

    /**
     * Creates a predicate based on the provided value and field type.
     *
     * @param builder The criteria builder used to construct the query.
     * @param path The path to the field in the entity.
     * @param value The value to be matched against the field.
     * @param fieldType The type of the field.
     * @return A predicate representing the filter criteria for the field.
     */
    @SuppressWarnings("unchecked") // Made sure these casts are safe
    private static Predicate createPredicate(CriteriaBuilder builder, Path path, Object value, Class<?> fieldType) {
        if (value instanceof Map) {
            Map<String, Object> valueMap = (Map<String, Object>) value;
            /* String operations.*/
            if (valueMap.containsKey(Operator.STARTS.getName())) {
                return builder.like(path, valueMap.get(Operator.STARTS.getName()) + "%");
            }
            else if (valueMap.containsKey(Operator.STARTSIC.getName())) {
                return builder.like(builder.lower(path), ((String) valueMap.get(Operator.STARTSIC.getName())).toLowerCase() + "%");
            }
            else if (valueMap.containsKey(Operator.ENDS.getName())) {
                return builder.like(path, "%" + valueMap.get(Operator.ENDS.getName()));
            }
            else if (valueMap.containsKey(Operator.ENDSIC.getName())) {
                return builder.like(builder.lower(path), "%" + ((String) valueMap.get(Operator.ENDSIC.getName())).toLowerCase());
            }
            else if (valueMap.containsKey(Operator.CONTAINS.getName())) {
                return builder.like(path, "%" + valueMap.get(Operator.CONTAINS.getName()) + "%");
            }
            else if (valueMap.containsKey(Operator.CONTAINSIC.getName())) {
                return builder.like(builder.lower(path), "%" + ((String)valueMap.get(Operator.CONTAINSIC.getName())).toLowerCase() + "%");
            }
            else if (valueMap.containsKey(Operator.EQUALS.getName())) {
                return builder.equal(path, valueMap.get(Operator.EQUALS.getName()));
            }
            else if (valueMap.containsKey(Operator.EQUALSIC.getName())) {
                return builder.equal(builder.lower(path), ((String)valueMap.get(Operator.EQUALSIC.getName())).toLowerCase());
            }
            /* Numeric operations.*/
            else if (valueMap.containsKey(Operator.LT.getName())) {
                Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.LT.getName()));
                return builder.lessThan(path, comparableValue);
            }
            else if (valueMap.containsKey(Operator.LTE.getName())) {
                Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.LTE.getName()));
                return builder.lessThanOrEqualTo(path, comparableValue);
            }
            else if (valueMap.containsKey(Operator.EQ.getName())) {
                if (valueMap.get(Operator.EQ.getName()) == null) {
                    return builder.isNull(path);
                } else {
                    Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.EQ.getName()));
                    return  builder.equal(path, comparableValue);
                }
            }
            else if (valueMap.containsKey(Operator.GT.getName())) {
                Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.GT.getName()));
                return builder.greaterThan(path, comparableValue);
            }
            else if (valueMap.containsKey(Operator.GTE.getName())) {
                Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.GTE.getName()));
                return builder.greaterThanOrEqualTo(path, comparableValue);
            }
            else if (valueMap.containsKey(Operator.IN.getName())) {
                List<Comparable> expressionInValues = (List<Comparable>) valueMap.get(Operator.IN.getName());
                List<Comparable> convertedValues = new ArrayList<>();
                for (Comparable val : expressionInValues) {
                    convertedValues.add(convertIfDate(val));
                }
                return builder.in(path).value(convertedValues);
            }
            else if (valueMap.containsKey(Operator.BETWEEN.getName())) {
                List<Comparable> expressionBetweenValues = (List<Comparable>) valueMap.get(Operator.BETWEEN.getName());
                Comparable start = convertIfDate(expressionBetweenValues.get(0));
                Comparable end = convertIfDate(expressionBetweenValues.get(1));
                return builder.between(path, start, end);
            }
            else
                return builder.equal(path, value);
        }
        return builder.equal(path, convertValue(value, fieldType));
    }

    /**
     * Converts the provided value to the specified field type.
     *
     * @param value The value to be converted.
     * @param fieldType The class of the field type to which the value should be converted.
     * @return The converted value, or the original value if no conversion is needed.
     */
    private static Object convertValue(Object value, Class<?> fieldType) {
        if (fieldType.equals(UUID.class) && value instanceof String) {
            return UUID.fromString((String) value);
        }
        //We can add more types here later if needed
        return value;
    }

    /**
     * Retrieves the path to a field in the entity based on the provided parent path and key.
     *
     * @param root The root entity in the query.
     * @param parentPath The parent path for nested fields.
     * @param key The key representing the field name.
     * @param rootClass The class of the root entity.
     * @return The path to the specified field in the entity.
     */
    private static Path<?> getPath(From<?, ?> root, String parentPath, String key, Class<?> rootClass) {
        if (parentPath.isEmpty()) {
            return root.get(getActualFieldName(rootClass, key));
        } else {
            String[] paths = parentPath.split("\\.");
            Path<?> path = root;
            for (String p : paths) {
                path = path.get(p);
            }
            return path.get(getActualFieldName(rootClass, key));
        }
    }

    /**
     * Checks if the provided key represents a logical operator.
     *
     * @param key The key to be checked.
     * @return true if the key represents a logical operator, false otherwise.
     */
    private static boolean isLogicalOperator(String key) {
        try {
            Operator operator = Operator.getOperator(key);
            return Objects.equals(operator.getType(), "Logical");
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    /**
     * Determines the root operator for combining predicates based on the provided filter map.
     *
     * @param filterMap A map containing filter criteria.
     * @return The root operator as a string (e.g., "and", "or"). Defaults to "and" if no logical operator is found.
     */
    private static String determineRootOperator(Map<String, Object> filterMap) {
        for (String key : filterMap.keySet()) {
            if (isLogicalOperator(key)) {
                return key.toLowerCase();
            }
        }
        return "and";
    }

    /**
     * Combines a list of predicates using the specified logical operator.
     *
     * @param <T> The type of the root entity.
     * @param predicates A list of predicates to be combined.
     * @param builder The criteria builder used to construct the query.
     * @param operator The logical operator to be applied (e.g., AND, OR, NOT).
     * @param query The criteria query to which the combined predicate will be applied.
     * @param root The root entity in the query.
     * @return A combined predicate representing the logical operation.
     */
    private static <T> Predicate combinePredicates(@NotNull List<Predicate> predicates, CriteriaBuilder builder, String operator, CriteriaQuery<?> query, Root<T> root) {
        Predicate combinedPredicate = null;

        for (Predicate predicate : predicates) {
            if (combinedPredicate == null) {
                combinedPredicate = predicate;
            } else {
                Operator op = Operator.getOperator(operator);
                combinedPredicate = switch (op) {
                    case AND -> builder.and(combinedPredicate, predicate);
                    case OR -> builder.or(combinedPredicate, predicate);
                    case NOT -> builder.not(combinedPredicate);
                    //case DISTINCT -> combinedPredicate;
                    default -> combinedPredicate;
                };
            }
        }

        return combinedPredicate;
    }

    /**
     * Checks if the specified field in the given class is a complex field.
     *
     * A complex field is defined as a field that is not primitive, not a String,
     * not overridden, and not one of the following types: UUID, Date, LocalDate,
     * LocalDateTime, OffsetDateTime.
     *
     * @param clazz The class containing the field.
     * @param fieldName The name of the field to check.
     * @return true if the field is complex, false otherwise.
     * @throws NoSuchFieldException If the field is not found in the class.
     */
    private static boolean isComplexField(Class<?> clazz, String fieldName) throws NoSuchFieldException {
        try {
            Class<?> fieldType = getActualFieldType(clazz, fieldName);
            return !fieldType.isPrimitive()
                    && !fieldType.equals(String.class)
                    && !isOverriddenField(clazz, fieldName)
                    && !fieldType.equals(UUID.class)
                    && !fieldType.equals(Date.class)
                    && !fieldType.equals(LocalDate.class)
                    && !fieldType.equals(LocalDateTime.class)
                    && !fieldType.equals(OffsetDateTime.class);
        } catch (NoSuchFieldException e) {
            throw new NoSuchFieldException(fieldName);
        }
    }

    /**
     * Checks if the specified field in the given class is overridden.
     *
     * This method traverses the class hierarchy to check for the presence of the @AttributeOverride annotation
     * on the specified field. If the annotation is found, it indicates that the field is overridden.
     *
     * @param clazz The class containing the field.
     * @param fieldName The name of the field to check.
     * @return true if the field is overridden, false otherwise.
     */
    private static boolean isOverriddenField(Class<?> clazz, String fieldName) {
        // Check for @AttributeOverride
        while (clazz != null) {
            for (AttributeOverride override : clazz.getDeclaredAnnotationsByType(AttributeOverride.class)) {
                if (override.name().equals(fieldName)) {
                    return true;
                }
            }
            clazz = clazz.getSuperclass();
        }
        return false;
    }

    /**
     * Retrieves the type of the specified field in the given class.
     *
     * This method attempts to get the actual field type of the specified field in the provided class.
     * If the field is not found, it throws a RuntimeException.
     *
     * @param clazz The class containing the field.
     * @param fieldName The name of the field whose type is to be retrieved.
     * @return The type of the specified field.
     * @throws RuntimeException If the field is not found in the class.
     */
    private static Class<?> getFieldType(Class<?> clazz, String fieldName) {
        try {
            return getActualFieldType(clazz, fieldName);
        } catch (NoSuchFieldException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Retrieves the actual type of the specified field in the given class.
     *
     * This method traverses the class hierarchy to find the field with the specified name
     * and returns its type. If the field is not found, it throws a NoSuchFieldException.
     *
     * @param clazz The class containing the field.
     * @param fieldName The name of the field whose type is to be retrieved.
     * @return The type of the specified field.
     * @throws NoSuchFieldException If the field is not found in the class.
     */
    private static Class<?> getActualFieldType(Class<?> clazz, String fieldName) throws NoSuchFieldException {
        // Traverse the class hierarchy to find the field
        while (clazz != null) {
            for (Field field : clazz.getDeclaredFields()) {
                if (field.getName().equals(fieldName)) {
                    return field.getType();
                }
            }
            clazz = clazz.getSuperclass();
        }
        throw new NoSuchFieldException(fieldName);
    }

    /**
     * Retrieves the actual field name of the specified field in the given class.
     *
     * This method traverses the class hierarchy to find the field with the specified name
     * and checks for the presence of the @AttributeOverride annotation. If the annotation
     * is found, it returns the overridden column name. If the field is not found, it returns
     * the original field name.
     *
     * @param clazz The class containing the field.
     * @param fieldName The name of the field whose actual name is to be retrieved.
     * @return The actual field name, or the original field name if no override is found.
     */
    private static String getActualFieldName(Class<?> clazz, String fieldName) {
        // Check for @AttributeOverride
        while (clazz != null) {
            for (AttributeOverride override : clazz.getDeclaredAnnotationsByType(AttributeOverride.class)) {
                if (override.name().equals(fieldName)) {
                    return override.column().name();
                }
            }
            clazz = clazz.getSuperclass();
        }
        return fieldName;
    }

    /**
     * Checks if the provided key represents an operator.
     *
     * This method attempts to retrieve an operator using the provided key.
     * If an exception occurs during the retrieval, it returns false.
     *
     * @param key The key to be checked.
     * @return true if the key represents an operator, false otherwise.
     */
    private static boolean isOperator(String key) {
        Operator operator = null;
        try {
            operator = Operator.getOperator(key);
        } catch (Exception ex) {

        }
        return operator != null;
    }

    /**
     * Converts the provided value to a `Date` if it is an instance of `LocalDate`, `LocalDateTime`, or `OffsetDateTime`.
     *
     * This method checks the type of the provided value and converts it to a `Date` object if it is a date-related type.
     * If the value is `null` or not a date-related type, it returns the original value.
     *
     * @param value The value to be converted.
     * @return The converted `Date` object if the value is a date-related type, otherwise the original value.
     */
    private static Comparable convertIfDate(Comparable value) {
        if (value == null) {
            return null;
        }
        if (value instanceof LocalDate) {
            LocalDate localDate = (LocalDate) value;
            value = Date.from(localDate.atStartOfDay()
                    .atZone(ZoneId.systemDefault())
                    .toInstant());
        } else if (value instanceof LocalDateTime) {
            LocalDateTime localDateTime = (LocalDateTime) value;
            value = Date
                    .from(localDateTime.atZone(ZoneId.systemDefault())
                            .toInstant());
        } else if (value instanceof OffsetDateTime) {
            OffsetDateTime offsetDateTime = (OffsetDateTime) value;
            value = Date
                    .from(offsetDateTime.toInstant());
        }
        return value;
    }

    /**
     * Cleans the list of fields by removing complex fields from the list.
     *
     * This method creates a copy of the provided fields list to avoid concurrent modification issues.
     * It iterates through the fields and removes any complex fields from the list.
     *
     * @param rootClass The class of the root entity.
     * @param fields A list of fields to be cleaned.
     * @return A list of cleaned fields with complex fields removed.
     * @throws NoSuchFieldException If a field specified in the fields list is not found in the root class.
     */
    public static List<String> CleanFields(Class<?> rootClass, List<String> fields) throws NoSuchFieldException {
        // Create a copy of the fields list to avoid concurrent modification issues
        if (fields == null)
            return new ArrayList<>();

        List<String> cleanedFields = new ArrayList<>(fields);

        for (int i = 0; i < cleanedFields.size(); i++) {
            String[] temp = cleanedFields.get(i).split("\\.");
            Class<?> currentClass = rootClass;  // Reset rootClass for each field entry
            boolean isComplex = false;

            // Start from the second element (index 1)
            for (int j = 1; j < temp.length; j++) {
                String fieldName = temp[j];

                if (isComplexField(currentClass, fieldName)) {
                    isComplex = true;  // Mark as complex if we encounter a complex field
                    // If we are at the last element and it's complex, remove the entry
                    if (j + 1 == temp.length) {
                        cleanedFields.remove(i);
                        i--;  // Adjust the index after removal
                        break;
                    } else {
                        // Update the current class to the complex field's type for further checks
                        currentClass = getFieldType(currentClass, fieldName);
                    }
                } else {
                    isComplex = false;  // The field is simple, no need to remove
                }
            }
        }

        return cleanedFields;
    }

    /**
     * Maps a list of `Tuple` objects to a list of maps.
     *
     * This method iterates over each `Tuple` in the provided list and converts it to a map.
     * Each map entry corresponds to a field in the `Tuple`, with the field alias as the key
     * and the field value as the value.
     *
     * @param tuples A list of `Tuple` objects to be converted.
     * @return A list of maps, where each map represents a `Tuple` with field aliases as keys and field values as values.
     */
    public List<Map<String, Object>> mapTuplesToMap(List<Tuple> tuples) {
        List<Map<String, Object>> result = new ArrayList<>();

        for (Tuple tuple : tuples) {
            Map<String, Object> map = new HashMap<>();

            // Iterate over the tuple and dynamically add entries to the map
            for (int i = 0; i < tuple.getElements().size(); i++) {
                String alias = tuple.getElements().get(i).getAlias();  // Get the alias of the field
                Object value = tuple.get(i);  // Get the value of the field by index

                map.put(alias, value);  // Map the alias to the field value
            }

            result.add(map);
        }

        return result;
    }


    // For Generic Entity Mapper
    public <T> List<T> executeAndMap(EntityManager em, String queryString, Class<T> entityClass) {
        TypedQuery<Tuple> query = em.createQuery(queryString, Tuple.class);
        List<Tuple> results = query.getResultList();

        GenericEntityMapper<T> mapper = new GenericEntityMapper<>();
        return results.stream()
                .map(result -> mapper.mapToEntity(result, entityClass))
                .collect(Collectors.toList());
    }
}
