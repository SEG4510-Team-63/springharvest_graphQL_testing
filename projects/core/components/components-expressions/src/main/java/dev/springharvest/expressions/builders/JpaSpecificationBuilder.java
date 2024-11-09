package dev.springharvest.expressions.builders;

import dev.springharvest.expressions.helpers.Operator;
import jakarta.persistence.AttributeOverride;
import jakarta.persistence.criteria.*;
import org.springframework.data.jpa.domain.Specification;

import java.lang.reflect.Field;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.*;

/**
 * This class is responsible for helping build JPA Specifications from a filter map.
 * It provides methods to parse filter expressions and create specifications for querying entities.
 *
 * @see Specification
 * @see Operator
 * @author Gilles Djawa (NeroNemesis)
 * @since 1.0
 */
public class JpaSpecificationBuilder {

    /**
     * Parses a filter expression map and creates a JPA Specification.
     *
     * @param filterMap the filter criteria as a map of field names to values
     * @param rootClass the class type of the root entity
     * @param <T> the type of the root entity
     * @return a JPA Specification based on the filter criteria
     */
    public static <T> Specification<T> parseFilterExpression(Map<String, Object> filterMap, Class<T> rootClass, List<String> fields) {
        String rootOperator = determineRootOperator(filterMap);
        fields = CleanFields(rootClass, fields);
        System.out.println("---------2----------------");
        System.out.println(fields);
        return createSpecification(filterMap, rootClass, "", rootOperator, fields);
    }

    /**
     * Creates a JPA Specification based on the filter map, root class, parent path, and root operator.
     *
     * @param filterMap the filter criteria as a map of field names to values
     * @param rootClass the class type of the root entity
     * @param parentPath the parent path for nested fields
     * @param rootOperator the root logical operator (e.g., "and", "or")
     * @param <T> the type of the root entity
     * @return a JPA Specification based on the filter criteria
     */
    private static <T> Specification<T> createSpecification(Map<String, Object> filterMap, Class<T> rootClass, String parentPath, String rootOperator, List<String> fields) {
        List<Specification<T>> specifications = new ArrayList<>();

        for (Map.Entry<String, Object> entry : filterMap.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (isLogicalOperator(key)) {
                if (Operator.getOperator(key).getKind() != Operator.Kind.UNARY)
                    specifications.add(createLogicalOperatorSpecification(key, (List<Map<String, Object>>) value, rootClass, parentPath, fields));
                else
                    specifications.add(createUnaryOperatorSpecification((Map<String, Object>) value, rootClass, parentPath, fields));
            } else if (isComplexField(rootClass, key)) {
                String newPath = parentPath.isEmpty() ? key : parentPath + "." + key;
                Class<?> nestedClass = getFieldType(rootClass, key);
                String subOperator = determineRootOperator((Map<String, Object>) value); //and
                specifications.add(createSpecification((Map<String, Object>) value, (Class)nestedClass, newPath, subOperator, fields));
            } else {
                specifications.add(createSimpleFieldSpecification(key, value, parentPath, rootClass, fields));
            }
        }

        return combineSpecifications(specifications, rootOperator);
    }

    /**
     * Creates a logical operator specification (e.g., "and", "or") based on the operator and value.
     *
     * @param operator the logical operator
     * @param value the list of filter maps
     * @param rootClass the class type of the root entity
     * @param parentPath the parent path for nested fields
     * @param <T> the type of the root entity
     * @return a JPA Specification based on the logical operator and value
     */
    private static <T> Specification<T> createLogicalOperatorSpecification(String operator, List<Map<String, Object>> value, Class<T> rootClass, String parentPath, List<String> fields) {
        List<Specification<T>> subSpecifications = new ArrayList<>();
        for (Map<String, Object> subFilter : value) {
            String subOperator = determineRootOperator(subFilter);
            subSpecifications.add(createSpecification(subFilter, rootClass, parentPath, subOperator, fields));
        }

        return combineSpecifications(subSpecifications, operator);
    }

    /**
     * Creates a unary operator specification (e.g., "not") based on the value.
     *
     * @param value the filter map
     * @param rootClass the class type of the root entity
     * @param parentPath the parent path for nested fields
     * @param <T> the type of the root entity
     * @return a JPA Specification based on the unary operator and value
     */
    private static <T> Specification<T> createUnaryOperatorSpecification(Map<String, Object> value, Class<T> rootClass, String parentPath, List<String> fields) {
        Specification<T> spec = createSpecification(value, rootClass, parentPath, determineRootOperator(value), fields);
        return Specification.not(spec);
    }

    /**
     * Creates a simple field specification based on the key, value, parent path, and root class.
     *
     * @param key the field name
     * @param value the field value
     * @param parentPath the parent path for nested fields
     * @param rootClass the class type of the root entity
     * @param <T> the type of the root entity
     * @return a JPA Specification based on the field criteria
     */
    private static <T> Specification<T> createSimpleFieldSpecification(String key, Object value, String parentPath, Class<T> rootClass, List<String> fields) {
        return (root, query, builder) -> {
            Path<?> path = getPath(root, parentPath, key, rootClass);
            applyFieldProjection(query, root, fields);
            Predicate predicate = createPredicate(builder, path, value, getFieldType(rootClass, key));
            return predicate;
        };
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

    /**
     * Creates a predicate based on the criteria builder, path, value, and field type.
     *
     * @param builder the criteria builder
     * @param path the path to the field
     * @param value the field value
     * @param fieldType the field type
     * @return a predicate based on the field criteria
     */
    private static Predicate createPredicate(CriteriaBuilder builder, Path path, Object value, Class<?> fieldType) {
        if (value instanceof Map) {
            Map<String, Object> valueMap = (Map<String, Object>) value;
            if (valueMap.containsKey(Operator.STARTS.getName())) {
                return builder.like(path, valueMap.get(Operator.STARTS.getName()) + "%");
            } else if (valueMap.containsKey(Operator.STARTSIC.getName())) {
                return builder.like(builder.lower(path), ((String) valueMap.get(Operator.STARTSIC.getName())).toLowerCase() + "%");
            } else if (valueMap.containsKey(Operator.ENDS.getName())) {
                return builder.like(path, "%" + valueMap.get(Operator.ENDS.getName()));
            } else if (valueMap.containsKey(Operator.ENDSIC.getName())) {
                return builder.like(builder.lower(path), "%" + ((String) valueMap.get(Operator.ENDSIC.getName())).toLowerCase());
            } else if (valueMap.containsKey(Operator.CONTAINS.getName())) {
                return builder.like(path, "%" + valueMap.get(Operator.CONTAINS.getName()) + "%");
            } else if (valueMap.containsKey(Operator.CONTAINSIC.getName())) {
                return builder.like(builder.lower(path), "%" + ((String)valueMap.get(Operator.CONTAINSIC.getName())).toLowerCase() + "%");
            } else if (valueMap.containsKey(Operator.EQUALS.getName())) {
                return builder.equal(path, valueMap.get(Operator.EQUALS.getName()));
            } else if (valueMap.containsKey(Operator.EQUALSIC.getName())) {
                return builder.equal(builder.lower(path), ((String)valueMap.get(Operator.EQUALSIC.getName())).toLowerCase());
            } else if (valueMap.containsKey(Operator.LT.getName())) {
                Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.LT.getName()));
                return builder.lessThan(path, comparableValue);
            } else if (valueMap.containsKey(Operator.LTE.getName())) {
                Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.LTE.getName()));
                return builder.lessThanOrEqualTo(path, comparableValue);
            } else if (valueMap.containsKey(Operator.EQ.getName())) {
                if (valueMap.get(Operator.EQ.getName()) == null) {
                    return builder.isNull(path);
                } else {
                    Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.EQ.getName()));
                    return  builder.equal(path, comparableValue);
                }
            } else if (valueMap.containsKey(Operator.GT.getName())) {
                Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.GT.getName()));
                return builder.greaterThan(path, comparableValue);
            } else if (valueMap.containsKey(Operator.GTE.getName())) {
                Comparable comparableValue = convertIfDate((Comparable) valueMap.get(Operator.GTE.getName()));
                return builder.greaterThanOrEqualTo(path, comparableValue);
            } else if (valueMap.containsKey(Operator.IN.getName())) {
                List<Comparable> expressionInValues = (List<Comparable>) valueMap.get(Operator.IN.getName());
                List<Comparable> convertedValues = new ArrayList<>();
                for (Comparable val : expressionInValues) {
                    convertedValues.add(convertIfDate(val));
                }
                return builder.in(path).value(convertedValues);
            } else if (valueMap.containsKey(Operator.BETWEEN.getName())) {
                List<Comparable> expressionBetweenValues = (List<Comparable>) valueMap.get(Operator.BETWEEN.getName());
                Comparable start = convertIfDate(expressionBetweenValues.get(0));
                Comparable end = convertIfDate(expressionBetweenValues.get(1));
                return builder.between(path, start, end);
            } else {
                return builder.equal(path, value);
            }
        }
        return builder.equal(path, convertValue(value, fieldType));
    }

    /**
     * Converts the value to the appropriate type if necessary.
     *
     * @param value the value to convert
     * @param fieldType the field type
     * @return the converted value
     */
    private static Object convertValue(Object value, Class<?> fieldType) {
        if (fieldType.equals(UUID.class) && value instanceof String) {
            return UUID.fromString((String) value);
        }
        return value;
    }

    /**
     * Gets the path to the field based on the root, parent path, key, and root class.
     *
     * @param root the root entity
     * @param parentPath the parent path for nested fields
     * @param key the field name
     * @param rootClass the class type of the root entity
     * @return the path to the field
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
     * Determines if the key is a logical operator.
     *
     * @param key the key to check
     * @return true if the key is a logical operator, false otherwise
     */
    private static boolean isLogicalOperator(String key) {
        try {
            Operator operator = Operator.getOperator(key);
            return operator.getType() == "Logical";
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    /**
     * Determines the root operator from the filter map.
     *
     * @param filterMap the filter criteria as a map of field names to values
     * @return the root operator (e.g., "and", "or")
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
     * Combines a list of specifications using the specified operator.
     *
     * @param specifications the list of specifications
     * @param operator the logical operator (e.g., "and", "or")
     * @param <T> the type of the root entity
     * @return a combined specification
     */
    private static <T> Specification<T> combineSpecifications(List<Specification<T>> specifications, String operator) {
        Specification<T> result = null;
        for (Specification<T> spec : specifications) {
            if (result == null) {
                result = spec;
            } else {
                Operator op = Operator.getOperator(operator);
                result = switch (op) {
                    case AND -> Specification.where(result).and(spec);
                    case OR -> Specification.where(result).or(spec);
                    case DISTINCT -> (Specification<T>) Specification.where(distinct().and((Specification<Object>) Specification.where(result).and(spec)));
                    case NOT -> result.and(Specification.not(spec));
                    default -> result;
                };
            }
        }
        return result;
    }

    /**
     * Determines if the field is a complex field (i.e., not a primitive or simple type).
     *
     * @param clazz the class type of the entity
     * @param fieldName the field name
     * @return true if the field is a complex field, false otherwise
     */
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

    /**
     * Determines if the field is overridden in the class hierarchy.
     *
     * @param clazz the class type of the entity
     * @param fieldName the field name
     * @return true if the field is overridden, false otherwise
     */
    private static boolean isOverriddenField(Class<?> clazz, String fieldName) {
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
     * Gets the field type based on the class and field name.
     *
     * @param clazz the class type of the entity
     * @param fieldName the field name
     * @return the field type
     */
    private static Class<?> getFieldType(Class<?> clazz, String fieldName) {
        try {
            return getActualFieldType(clazz, fieldName);
        } catch (NoSuchFieldException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Gets the actual field type based on the class and field name.
     *
     * @param clazz the class type of the entity
     * @param fieldName the field name
     * @return the actual field type
     * @throws NoSuchFieldException if the field does not exist
     */
    private static Class<?> getActualFieldType(Class<?> clazz, String fieldName) throws NoSuchFieldException {
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
     * Gets the actual field name based on the class and field name.
     *
     * @param clazz the class type of the entity
     * @param fieldName the field name
     * @return the actual field name
     */
    private static String getActualFieldName(Class<?> clazz, String fieldName) {
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
     * Determines if the key is an operator.
     *
     * @param key the key to check
     * @return true if the key is an operator, false otherwise
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
     * Converts the value to a date if necessary.
     *
     * @param value the value to convert
     * @return the converted value
     */
    private static Comparable convertIfDate(Comparable value) {
        if (value == null) {
            return null;
        }
        if (value instanceof LocalDate) {
            LocalDate localDate = (LocalDate) value;
            value = java.util.Date.from(localDate.atStartOfDay()
                    .atZone(ZoneId.systemDefault())
                    .toInstant());
        } else if (value instanceof LocalDateTime) {
            LocalDateTime localDateTime = (LocalDateTime) value;
            value = java.util.Date
                    .from(localDateTime.atZone(ZoneId.systemDefault())
                            .toInstant());
        } else if (value instanceof OffsetDateTime) {
            OffsetDateTime offsetDateTime = (OffsetDateTime) value;
            value = java.util.Date
                    .from(offsetDateTime.toInstant());
        }
        return value;
    }

    /**
     * Creates a distinct specification.
     *
     * @param <T> the type of the root entity
     * @return a distinct specification
     */
    public static <T> Specification<T> distinct() {
        return (root, query, cb) -> {
            query.distinct(true);
            return null;
        };
    }

    public static List<String> CleanFields(Class<?> rootClass, List<String> fields) {
        // Create a copy of the fields list to avoid concurrent modification issues
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
