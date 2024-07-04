package dev.springharvest.expressions.helpers;

import dev.springharvest.expressions.ast.Operator;
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
 * This class is responsible for helping build
 * JPA Specifications from a filter map.
 *
 * @author NeroNemesis
 */
public class JpaSpecificationBuilder {
    public static <T> Specification<T> parseFilterExpression(Map<String, Object> filterMap, Class<T> rootClass) {
        String rootOperator = determineRootOperator(filterMap);
        return createSpecification(filterMap, rootClass, "", rootOperator);
    }

    private static <T> Specification<T> createSpecification(Map<String, Object> filterMap, Class<T> rootClass, String parentPath, String rootOperator) {
        List<Specification<T>> specifications = new ArrayList<>();

        for (Map.Entry<String, Object> entry : filterMap.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (isLogicalOperator(key)) {
                if (Operator.getOperator(key).getKind() != Operator.Kind.UNARY)
                    specifications.add(createLogicalOperatorSpecification(key, (List<Map<String, Object>>) value, rootClass, parentPath));
                else
                    specifications.add(createUnaryOperatorSpecification((Map<String, Object>) value, rootClass, parentPath));
            } else if (isComplexField(rootClass, key)) {
                System.out.println(key);
                String newPath = parentPath.isEmpty() ? key : parentPath + "." + key;
                Class<?> nestedClass = getFieldType(rootClass, key);
                String subOperator = determineRootOperator((Map<String, Object>) value);
                specifications.add(createSpecification((Map<String, Object>) value, (Class)nestedClass, newPath, subOperator));
            } else {
                specifications.add(createSimpleFieldSpecification(key, value, parentPath, rootClass));
            }
        }

        return combineSpecifications(specifications, rootOperator);
    }

    private static <T> Specification<T> createLogicalOperatorSpecification(String operator, List<Map<String, Object>> value, Class<T> rootClass, String parentPath) {
        List<Specification<T>> subSpecifications = new ArrayList<>();
        for (Map<String, Object> subFilter : value) {
            String subOperator = determineRootOperator(subFilter);
            subSpecifications.add(createSpecification(subFilter, rootClass, parentPath, subOperator));
        }

        return combineSpecifications(subSpecifications, operator);
    }

    private static <T> Specification<T> createUnaryOperatorSpecification(Map<String, Object> value, Class<T> rootClass, String parentPath) {
        Specification<T> spec = createSpecification(value, rootClass, parentPath, determineRootOperator(value));
        return Specification.not(spec);
    }

    private static <T> Specification<T> createSimpleFieldSpecification(String key, Object value, String parentPath, Class<T> rootClass) {
        return (root, query, builder) -> {
            Path<?> path = getPath(root, parentPath, key, rootClass);
            Predicate predicate = createPredicate(builder, path, value, getFieldType(rootClass, key));
            return predicate;
        };
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

    /*allows to perform distinct operations*/
    public static <T> Specification<T> distinct() {
        return (root, query, cb) -> {
            query.distinct(true);
            return null;
        };
    }
}
