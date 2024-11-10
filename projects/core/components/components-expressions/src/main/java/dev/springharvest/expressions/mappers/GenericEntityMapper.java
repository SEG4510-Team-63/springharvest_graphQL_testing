package dev.springharvest.expressions.mappers;

import jakarta.persistence.Tuple;
import java.lang.reflect.Field;
import java.util.Map;

public class GenericEntityMapper<T> implements EntityMapper<T> {

    @Override
    public T mapToEntity(Object rawObject, Class<T> entityClass) {
        try {
            T entity = entityClass.getDeclaredConstructor().newInstance();

            if (rawObject instanceof Tuple) {
                Tuple tuple = (Tuple) rawObject;
                for (Field field : entityClass.getDeclaredFields()) {
                    field.setAccessible(true);
                    Object value = tuple.get(field.getName(), field.getType());
                    if (value != null || field.getType().isPrimitive()) {
                        field.set(entity, value);
                    }
                }
            } else if (rawObject instanceof Map) {
                Map<String, Object> map = (Map<String, Object>) rawObject;
                for (Field field : entityClass.getDeclaredFields()) {
                    field.setAccessible(true);
                    Object value = map.get(field.getName());
                    if (value != null || field.getType().isPrimitive()) {
                        field.set(entity, value);
                    }
                }
            } else {
                throw new IllegalArgumentException("Data type not supported for mapping");
            }

            // Mapping fields inherited from the parent class
            if (entityClass.getSuperclass() != null) {
                mapInheritedFields(entity, rawObject, entityClass.getSuperclass());
            }

            return entity;
        } catch (Exception e) {
            throw new RuntimeException("Entity mapping error" + entityClass.getSimpleName(), e);
        }
    }

    private void mapInheritedFields(T entity, Object rawObject, Class<?> superclass) throws IllegalAccessException {
        for (Field field : superclass.getDeclaredFields()) {
            field.setAccessible(true);
            Object value = null;

            if (rawObject instanceof Tuple) {
                value = ((Tuple) rawObject).get(field.getName(), field.getType());
            } else if (rawObject instanceof Map) {
                value = ((Map<?, ?>) rawObject).get(field.getName());
            }

            if (value != null || field.getType().isPrimitive()) {
                field.set(entity, value);
            }
        }
    }
}
