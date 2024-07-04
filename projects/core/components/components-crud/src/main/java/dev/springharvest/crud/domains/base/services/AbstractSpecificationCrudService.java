package dev.springharvest.crud.domains.base.services;

import dev.springharvest.crud.domains.base.persistence.ICrudRepository;
import dev.springharvest.crud.domains.base.persistence.ISpecificationCrudRepository;
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

@Slf4j
public abstract class AbstractSpecificationCrudService<E extends BaseEntity<K>, K extends Serializable>
        implements ISpecificationCrudService<E, K> {

    protected ISpecificationCrudRepository<E, K> crudRepository;

    protected AbstractSpecificationCrudService(ISpecificationCrudRepository<E, K> crudRepository) {
        this.crudRepository = crudRepository;
    }

    protected Class<E> getClazz() {
        ParameterizedType paramType = (ParameterizedType) getClass().getGenericSuperclass();
        return (Class<E>) paramType.getActualTypeArguments()[1];
    }

    @Override
    @Transactional(readOnly = true)
    public boolean existsById(K id) {
        return crudRepository.existsById(id);
    }

    @Override
    @Transactional(readOnly = true)
    public List<E> findAll() {
        return crudRepository.findAll();
    }

    @Override
    @Transactional(readOnly = true)
    public Page<E> findAll(Pageable pageable) {
        return crudRepository.findAll(pageable);
    }

    @Override
    @Transactional(readOnly = true)
    public List<E> findAll(Specification<E> spec){ return crudRepository.findAll(spec); }

    @Override
    @Transactional(readOnly = true)
    public Page<E> findAll(Specification<E> spec, Pageable page) { return crudRepository.findAll(spec, page); }

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

    @Transactional
    public List<E> create(@Valid List<E> entities) {
        return afterCreation(crudRepository.saveAll(beforeCreation(entities)));
    }

    @Transactional
    public E update(@Valid E entity) {
        return afterUpdate(crudRepository.save(beforeUpdate(entity)));
    }

    @Transactional
    public List<E> update(@Valid List<E> entities) {
        return afterUpdate(entities.stream().map(this::update).toList());
    }

    @Transactional
    public void deleteById(K id) {
        crudRepository.deleteById(id);
    }

    @Transactional
    public void deleteById(List<K> ids) {
        crudRepository.deleteAllById(ids);
    }

    protected List<E> afterUpdate(List<E> source) {
        source.forEach(this::afterUpdate);
        return source;
    }

    protected E afterUpdate(E entity) {
        return entity;
    }

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

    protected void validate(E entity) {
        // TODO: implement update validation
    }

    protected List<E> beforeCreation(List<E> source) {
        source.forEach(this::beforeCreation);
        return source;
    }

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

    protected List<E> afterCreation(List<E> source) {
        source.forEach(this::afterCreation);
        return source;
    }

    protected E afterCreation(E entity) {
        return entity;
    }
}
