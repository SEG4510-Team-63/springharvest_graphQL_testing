package dev.springharvest.expressions.mappers;

public interface EntityMapper<T> {
    T mapToEntity(Object rawObject, Class<T> entityClass);
}
