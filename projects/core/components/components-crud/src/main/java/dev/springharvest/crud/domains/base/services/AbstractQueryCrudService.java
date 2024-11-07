package dev.springharvest.crud.domains.base.services;

import dev.springharvest.crud.domains.base.persistence.IQueryCrudRepository;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import dev.springharvest.shared.domains.embeddables.traces.dates.models.entities.TraceDatesEntity;
import dev.springharvest.shared.domains.embeddables.traces.trace.models.entities.TraceDataEntity;
import dev.springharvest.shared.domains.embeddables.traces.traceable.models.entities.ITraceableEntity;
import dev.springharvest.shared.domains.embeddables.traces.users.models.entities.TraceUsersEntity;
import jakarta.persistence.EntityExistsException;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.transaction.annotation.Transactional;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.List;

/**
 * Abstract service class providing CRUD operations with specifications.
 * This class is intended to be extended by specific service implementations.
 *
 * @param <E> The entity type, which extends BaseEntity<K>
 * @param <K> The type of the primary key field, which extends Serializable
 *
 * @see ISpecificationCrudService
 * @see BaseEntity
 * @since 1.0
 */
@Slf4j
public abstract class AbstractQueryCrudService<E extends BaseEntity<K>, K extends Serializable>
        implements IQueryCrudService<E, K> {

    /**
     * Repository to handle CRUD operations with Typed queries or specifications.
     */
    protected IQueryCrudRepository<E, K> crudRepository;

    /**
     * Constructs an AbstractQueryCrudService with the specified repository.
     *
     * @param crudRepository the repository to handle CRUD operations
     */
    protected AbstractQueryCrudService(IQueryCrudRepository<E, K> crudRepository) {
        this.crudRepository = crudRepository;
    }

    /**
     * Gets the class type of the entity.
     *
     * @return the class type of the entity
     */
    protected Class<E> getClazz() {
        ParameterizedType paramType = (ParameterizedType) getClass().getGenericSuperclass();
        return (Class<E>) paramType.getActualTypeArguments()[1];
    }

    /**
     * Checks if an entity with the specified ID exists.
     *
     * @param id the ID of the entity
     * @return true if the entity exists, false otherwise
     */
    @Override
    @Transactional(readOnly = true)
    public boolean existsById(K id) {
        return crudRepository.existsById(id);
    }

    /**
     * Finds all entities.
     *
     * @return a list of all entities
     */
    @Override
    @Transactional(readOnly = true)
    public List<E> findAll() {
        return crudRepository.findAll();
    }

    /**
     * Finds all entities with pagination.
     *
     * @param pageable the pagination information
     * @return a page of entities
     */
    @Override
    @Transactional(readOnly = true)
    public Page<E> findAll(Pageable pageable) {
        return crudRepository.findAll(pageable);
    }

    /**
     * Finds all entities matching the specified specification.
     *
     * @param spec the specification to filter entities
     * @return a list of entities matching the specification
     */
    @Override
    @Transactional(readOnly = true)
    public List<E> findAll(Specification<E> spec) {
        return crudRepository.findAll(spec);
    }

    /**
     * Finds all entities matching the specified specification with pagination.
     *
     * @param spec the specification to filter entities
     * @param page the pagination information
     * @return a page of entities matching the specification
     */
    @Override
    @Transactional(readOnly = true)
    public Page<E> findAll(Specification<E> spec, Pageable page) {
        return crudRepository.findAll(spec, page);
    }

    /**
     * Creates a new entity.
     *
     * @param entity the entity to create
     * @return the created entity
     */
    @Transactional
    public E create(@Valid E entity) {
        entity = beforeCreation(entity);
        try {
            return afterCreation(crudRepository.save(entity));
        } catch (EntityExistsException e) {
            log.error("Entity already exists", e);
            throw e;
        }
    }

    /**
     * Creates a list of new entities.
     *
     * @param entities the entities to create
     * @return the created entities
     */
    @Transactional
    public List<E> create(@Valid List<E> entities) {
        return afterCreation(crudRepository.saveAll(beforeCreation(entities)));
    }

    /**
     * Updates an existing entity.
     *
     * @param entity the entity to update
     * @return the updated entity
     */
    @Transactional
    public E update(@Valid E entity) {
        return afterUpdate(crudRepository.save(beforeUpdate(entity)));
    }

    /**
     * Updates a list of existing entities.
     *
     * @param entities the entities to update
     * @return the updated entities
     */
    @Transactional
    public List<E> update(@Valid List<E> entities) {
        return afterUpdate(entities.stream().map(this::update).toList());
    }

    /**
     * Deletes an entity by its ID.
     *
     * @param id the ID of the entity to delete
     */
    @Transactional
    public void deleteById(K id) {
        crudRepository.deleteById(id);
    }

    /**
     * Deletes entities by their IDs.
     *
     * @param ids the IDs of the entities to delete
     */
    @Transactional
    public void deleteById(List<K> ids) {
        crudRepository.deleteAllById(ids);
    }

    /**
     * Hook method called after updating a list of entities.
     *
     * @param source the list of entities
     * @return the list of entities after update
     */
    protected List<E> afterUpdate(List<E> source) {
        source.forEach(this::afterUpdate);
        return source;
    }

    /**
     * Hook method called after updating an entity.
     *
     * @param entity the entity
     * @return the entity after update
     */
    protected E afterUpdate(E entity) {
        return entity;
    }

    /**
     * Hook method called before updating an entity.
     *
     * @param source the entity
     * @return the entity before update
     */
    protected E beforeUpdate(E source) {
        validate(source);
        K id = source.getId();
        if (!existsById(id)) {
            return create(source);
        }
        if (source instanceof ITraceableEntity<?>) {
            TraceDataEntity<K> traceData = ((ITraceableEntity) source).getTraceData();
            if (traceData != null) {
                if (ObjectUtils.isNotEmpty(traceData.getTraceDates())) {
                    TraceDatesEntity traceDates = traceData.getTraceDates();
                    final LocalDate utcTimeStamp = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
                    traceDates.setDateUpdated(utcTimeStamp);
                    traceData.setTraceDates(traceDates);
                }
                if (ObjectUtils.isNotEmpty(traceData.getTraceUsers())) {
                    TraceUsersEntity traceUsers = traceData.getTraceUsers();
                    // For Now
                    log.info("TODO: Place holder for setting created user.");
                    traceData.setTraceUsers(traceUsers);
                }
            }
        }
        return source;
    }

    /**
     * Validates the entity before update.
     *
     * @param entity the entity to validate
     */
    protected void validate(E entity) {
        // TODO: implement update validation
    }

    /**
     * Hook method called before creating a list of entities.
     *
     * @param source the list of entities
     * @return the list of entities before creation
     */
    protected List<E> beforeCreation(List<E> source) {
        source.forEach(this::beforeCreation);
        return source;
    }

    /**
     * Hook method called before creating an entity.
     *
     * @param entity the entity
     * @return the entity before creation
     */
    protected E beforeCreation(E entity) {
        entity.setId(null);

        if (entity instanceof ITraceableEntity<?>) {
            final LocalDate utcTimeStamp = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
            ((ITraceableEntity<Serializable>) entity).setTraceData(TraceDataEntity.builder()
                    .traceDates(TraceDatesEntity.builder()
                            .dateCreated(utcTimeStamp)
                            .dateUpdated(utcTimeStamp)
                            .build())
                    .traceUsers(TraceUsersEntity.builder()
                            // TODO
                            .build())
                    .build());
        }

        return entity;
    }

    /**
     * Hook method called after creating a list of entities.
     *
     * @param source the list of entities
     * @return the list of entities after creation
     */
    protected List<E> afterCreation(List<E> source) {
        source.forEach(this::afterCreation);
        return source;
    }

    /**
     * Hook method called after creating an entity.
     *
     * @param entity the entity
     * @return the entity after creation
     */
    protected E afterCreation(E entity) {
        return entity;
    }
}