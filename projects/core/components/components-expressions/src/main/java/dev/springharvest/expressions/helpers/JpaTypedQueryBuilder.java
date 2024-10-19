package dev.springharvest.expressions.helpers;

import dev.springharvest.expressions.ast.Operation;
import dev.springharvest.expressions.ast.Operator;
import jakarta.persistence.*;
import jakarta.persistence.criteria.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.testcontainers.shaded.org.bouncycastle.oer.Switch;

import java.lang.reflect.Field;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.*;

/**
 * This class is responsible for helping build
 * JPA Specifications from a filter map.
 *
 * @author NeroNemesis
 */
@Component
public class JpaTypedQueryBuilder {

    @Autowired
    private EntityManagerFactory entityManagerFactory;

    public <T> List<Object> parseFilterExpression(Map<String, Object> filterMap, Map<String, Object> operationMap, Class<T> rootClass, List<String> fields, Operation operation) {
        EntityManager entityManager = entityManagerFactory.createEntityManager();

        try {
            fields = CleanFields(rootClass, fields);
            CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
            CriteriaQuery<Object> criteriaQuery = criteriaBuilder.createQuery(Object.class);
            Root<T> root = criteriaQuery.from(rootClass);

            if (filterMap == null || filterMap.isEmpty()) {
                return applyOperations(criteriaBuilder, criteriaQuery, entityManager, null, operationMap, root, fields, operation, null, rootClass, null);
            }

            String rootOperator = determineRootOperator(filterMap);
            Predicate predicate = createTypedQuery(filterMap, criteriaBuilder, rootClass, root, "", rootOperator, fields, criteriaQuery);
            criteriaQuery.where(predicate);

            return applyOperations(criteriaBuilder, criteriaQuery, entityManager, predicate, operationMap, root, fields, operation, filterMap, rootClass, rootOperator);

        } finally {
            if (entityManager != null && entityManager.isOpen()) {
                entityManager.close();
            }
        }
    }

    private <T> List<Object> applyOperations(CriteriaBuilder criteriaBuilder, CriteriaQuery<Object> criteriaQuery, EntityManager entityManager, Predicate predicate, Map<String, Object> operationMap, Root<T> root, List<String> fields, Operation operation, Map<String, Object> filterMap, Class<T> rootClass, String rootOperator) {
        TypedQuery<Object> typedQuery;

        // If there are no operations, return the query results as is

        if (operationMap == null || operationMap.isEmpty()) {
            if (operation == Operation.SEARCH) {
                applyFieldProjection(criteriaQuery, root, fields);
                typedQuery = entityManager.createQuery(criteriaQuery);
                return typedQuery.getResultList();
            }
            else if (operation == Operation.COUNT) {
                CriteriaQuery<Long> countQuery = criteriaBuilder.createQuery(Long.class);
                countQuery.select(criteriaBuilder.count(criteriaQuery.getRoots().iterator().next()));
                if (predicate != null) countQuery.where(predicate); // We apply predicate if present
                return List.of(entityManager.createQuery(countQuery).getSingleResult());
            }
            //Other future operations can be added here
            else
                throw new IllegalArgumentException("No such operation");
        }

        boolean distinct = false;

        // Extract distinct and count operations from the operationMap
        for (Map.Entry<String, Object> entry : operationMap.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            Operator op = Operator.getOperator(key);

            switch (op) {
                case DISTINCT -> distinct = (boolean) value;
                default -> throw new IllegalArgumentException("Not an operational operator");
            }
        }

        if (operation == Operation.SEARCH) {
            if (distinct) {
                applyFieldProjection(criteriaQuery, root, fields);
                criteriaQuery.distinct(true);
                typedQuery = entityManager.createQuery(criteriaQuery);
                return typedQuery.getResultList();
            }
            //if clauses other than distinct are added in the future handle them here
            else {
                applyFieldProjection(criteriaQuery, root, fields);
                typedQuery = entityManager.createQuery(criteriaQuery);
                return typedQuery.getResultList();
            }
        }
        else if (operation == Operation.COUNT) {
            CriteriaQuery<Long> countQuery = criteriaBuilder.createQuery(Long.class);
            if (distinct) {
                //Subquery<Long> subquery = countQuery.subquery(Long.class);
                Root<?> subRoot = countQuery.from(root.getJavaType()); // Use root entity type for counting distinct
                //countQuery.select(criteriaBuilder.countDistinct(subRoot)); // Select distinct root entities for the count
                Predicate pr = filterMap != null ? createTypedQuery(filterMap, criteriaBuilder, rootClass, (Root) subRoot, "", rootOperator, fields, criteriaQuery) : null;


                countQuery.select(criteriaBuilder.countDistinct(subRoot));
                if (pr != null) {
                    countQuery.where(pr); // Apply filtering conditions
                }
            } else {
                Root<?> subRoot = countQuery.from(root.getJavaType()); // Use root entity type for counting distinct
                countQuery.select(criteriaBuilder.count(subRoot)); // Regular count without distinct

                Predicate pr = filterMap != null ? createTypedQuery(filterMap, criteriaBuilder, rootClass, (Root) subRoot, "", rootOperator, fields, criteriaQuery) : null;
                if (pr != null) {
                    countQuery.where(pr); // Apply filtering conditions
                }
            }

            return List.of(entityManager.createQuery(countQuery).getSingleResult());
        }
        else
            throw new IllegalArgumentException("No such operation.");
    }

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
            } else if (isComplexField(rootClass, key)) {
                String newPath = parentPath.isEmpty() ? key : parentPath + "." + key;
                Class<?> nestedClass = getFieldType(rootClass, key);
                String subOperator = determineRootOperator((Map<String, Object>) value);
                predicates.add(createTypedQuery((Map<String, Object>) value, builder, (Class) nestedClass, root, newPath, subOperator, fields, query));
            } else {
                predicates.add(createSimpleFieldTypedQuery(key, value, parentPath, rootClass, builder, root, fields));
            }
        }

        return combinePredicates(predicates, builder, rootOperator, query, root);
    }

    private static <T> Predicate createLogicalOperatorTypedQuery(String operator, List<Map<String, Object>> value, CriteriaBuilder builder, Class<T> rootClass, Root<T> root, String parentPath, List<String> fields, CriteriaQuery<?> query) {
        List<Predicate> subPredicates = new ArrayList<>();

        for (Map<String, Object> subFilter : value) {
            String subOperator = determineRootOperator(subFilter);
            subPredicates.add(createTypedQuery(subFilter, builder, rootClass, root, parentPath, subOperator, fields, query));
        }

        return combinePredicates(subPredicates, builder, operator, query, root);
    }

    private static <T> Predicate createUnaryOperatorTypedQuery(Map<String, Object> value, CriteriaBuilder builder, Class<T> rootClass, Root<T> root, String parentPath, List<String> fields, CriteriaQuery<?> query) {
        Predicate predicate = createTypedQuery(value, builder, rootClass, root, parentPath, determineRootOperator(value), fields, query);
        return builder.not(predicate);
    }

    private static <T> Predicate createSimpleFieldTypedQuery(String key, Object value, String parentPath, Class<T> rootClass, CriteriaBuilder builder, Root<T> root, List<String> fields) {
        Path<?> path = getPath(root, parentPath, key, rootClass);
        return createPredicate(builder, path, value, getFieldType(rootClass, key));
    }

    private static void applyFieldProjection(CriteriaQuery<?> query, Root<?> root, List<String> fields) {
        List<Selection<?>> selections = new ArrayList<>();

        for (String field : fields) {
            Path<?> path = getPathFromField(root, field);
            selections.add(path);
        }

        query.multiselect(selections); // Apply projection to select only specified fields
    }

    private static Path<?> getPathFromField(Root<?> root, String field) {
        String[] pathParts = field.split("\\.");
        Path<?> path = root;

        for (int i = 1; i < pathParts.length; i++) {
            path = path.get(pathParts[i]);
        }

        return path;
    }

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

    private static Object convertValue(Object value, Class<?> fieldType) {
        if (fieldType.equals(UUID.class) && value instanceof String) {
            return UUID.fromString((String) value);
        }
        //We can add more types here later if needed
        return value;
    }

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

    private static boolean isLogicalOperator(String key) {
        try {
            Operator operator = Operator.getOperator(key);
            return operator.getType() == "Logical";
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private static String determineRootOperator(Map<String, Object> filterMap) {
        for (String key : filterMap.keySet()) {
            if (isLogicalOperator(key)) {
                return key.toLowerCase();
            }
        }
        return "and";
    }

    private static <T> Predicate combinePredicates(List<Predicate> predicates, CriteriaBuilder builder, String operator, CriteriaQuery<?> query, Root<T> root) {
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
                    case DISTINCT -> combinedPredicate;
                    default -> combinedPredicate;
                };
            }
        }

        return combinedPredicate;
    }

    private static boolean isComplexField(Class<?> clazz, String fieldName) {
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
            throw new RuntimeException(e);
        }
    }

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

    private static Class<?> getFieldType(Class<?> clazz, String fieldName) {
        try {
            return getActualFieldType(clazz, fieldName);
        } catch (NoSuchFieldException e) {
            throw new RuntimeException(e);
        }
    }

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

    private static boolean isOperator(String key) {
        Operator operator = null;
        try {
            operator = Operator.getOperator(key);
        } catch (Exception ex) {

        }
        return operator != null;
    }

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

    public static List<String> CleanFields(Class<?> rootClass, List<String> fields) {
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
}
