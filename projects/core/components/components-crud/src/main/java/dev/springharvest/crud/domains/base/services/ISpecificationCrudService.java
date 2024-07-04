package dev.springharvest.crud.domains.base.services;

import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.Serializable;
import java.util.List;

/**
 * The interface used to define the contract for a base service that uses specifications.
 *
 * @param <E> The type of the Entity.
 * @param <K> The type of the id (primary key) field.
 */
public interface ISpecificationCrudService<E extends BaseEntity<K>, K extends Serializable> {
    /**
     * Returns true if an entity with the given id exists, false otherwise.
     *
     * @param id The id of the entity to check for existence.
     * @return True if an entity with the given id exists, false otherwise.
     */
    boolean existsById(K id);
    /**
     * Returns all entities of the entity domain.
     *
     * @return All entities of the entity domain.
     */
    List<E> findAll();
    /**
     * Returns all entities of the entity domain.
     *
     * @return All entities of the entity domain.
     */
    Page<E> findAll(Pageable pageable);

    /**
     * Returns all entities of the entity domain.
     *
     * @return All entities of the entity domain.
     */
    List<E> findAll(Specification<E> spec);

    /**
     * Returns all entities of the entity domain.
     *
     * @return All entities of the entity domain.
     */
    Page<E> findAll(Specification<E> spec, Pageable page);

    /**
     * Saves a new entity to the database if it does not already exist.
     *
     * @param dto The DTO to save.
     * @return The saved DTO.
     */
    E create(E dto);

    /**
     * Saves a list of new entities to the database if they do not already exist.
     *
     * @param dtos The list of DTOs to save.
     * @return The list of saved DTOs.
     */
    List<E> create(List<E> dtos);

    /**
     * Updates an existing entity in the database.
     *
     * @param dto The DTO to update.
     * @return The updated DTO.
     */
    E update(E dto);

    /**
     * Updates a list of existing entities in the database.
     *
     * @param dtos
     * @return The list of updated DTOs.
     */
    List<E> update(List<E> dtos);

    /**
     * Deletes an entity from the database.
     *
     * @param id The id of the entity to delete.
     */
    void deleteById(K id);

    /**
     * Deletes a list of entities from the database.
     *
     * @param ids The list of ids of the entities to delete.
     */
    void deleteById(List<K> ids);
}
