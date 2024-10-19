package dev.springharvest.crud.domains.base.graphql;

import dev.springharvest.crud.domains.base.services.AbstractCrudService;
import dev.springharvest.crud.domains.base.services.AbstractSpecificationCrudService;
import dev.springharvest.expressions.ast.Operation;
import dev.springharvest.expressions.helpers.JpaSpecificationBuilder;
import dev.springharvest.expressions.helpers.JpaTypedQueryBuilder;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import dev.springharvest.shared.domains.base.models.dtos.BaseDTO;
import dev.springharvest.shared.domains.base.models.entities.BaseEntity;
import graphql.schema.DataFetchingEnvironment;
import graphql.schema.SelectedField;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import java.io.Serializable;
import java.util.*;

/**
 * A generic implementation of the IGraphQLCrudController interface.
 *
 * @param <D> The DTO type
 * @param <E> The entity type
 * @param <K> The type of the id (primary key) field
 * @author Gilles Djawa (NeroNemesis)
 * @see IGraphQLCrudController
 * @see AbstractCrudService
 * @see BaseDTO
 * @see BaseEntity
 * @since 1.0
 */
public class AbstractGraphQLCrudController<D extends BaseDTO<K>, E extends BaseEntity<K>, K extends Serializable>
        implements IGraphQLCrudController<D, K> {

    protected IBaseModelMapper<D, E, K> modelMapper;
    protected AbstractSpecificationCrudService<E, K> crudService;
    protected Class<E> entityClass;
    protected List<String> Fields;
    @Autowired
    private JpaTypedQueryBuilder TypedQueryBuilder;

    protected AbstractGraphQLCrudController(IBaseModelMapper<D, E, K> modelMapper,
                                            AbstractSpecificationCrudService<E, K> crudService,
                                            Class<E> entityClass) {
        this.modelMapper = modelMapper;
        this.crudService = crudService;
        this.entityClass = entityClass;
        this.Fields = new ArrayList<>();
    }

    @Override
    public List<D> search(Map<String, Object> filter, Map<String, Object> operation, DataPaging paging, DataFetchingEnvironment environment) {
        var pageRequest = PageRequest.of(paging.page(), paging.size(), paging.sortDirection().name().equals("A") ? Sort.by(paging.sortOrders()).ascending() : Sort.by(paging.sortOrders()).descending());

//        if (filter.isEmpty()) {
//            if (operation.isEmpty())
//            {
//                Page<E> page = crudService.findAll(pageRequest);
//                Page<D> dtos = page.hasContent() ? modelMapper.pagedEntityToPagedDto(page) : Page.empty(pageRequest);
//                return dtos.getContent();
//            }
//        }

        environment.getSelectionSet().getFields().forEach(x -> Fields.add(x.getFullyQualifiedName()));
//        System.out.println("Search 1");
//        System.out.println(Fields);
//
//        System.out.println("-----------------------------------");
        //System.out.println(getFormattedFields(Fields));
        List<String> fields2 = Fields;
        Specification<E> specification = JpaSpecificationBuilder.parseFilterExpression(filter, entityClass, getFormattedFields(Fields));
        List<Object> things = TypedQueryBuilder.parseFilterExpression(filter, operation, entityClass, getFormattedFields(fields2), Operation.SEARCH);
        System.out.println("things : " + things);
        System.out.println("things length : " + things.size());

        for (Object result : things) {
            Object [] result2 = (Object[]) result;
            for (Object o : result2) {
                System.out.println("i : " + ((String) o.toString()));
            }
        }
        Page<E> page = crudService.findAll(specification, pageRequest);
        Page<D> dtos = page.hasContent() ? modelMapper.pagedEntityToPagedDto(page) : Page.empty(pageRequest);

        return dtos.getContent();
    }

    @Override
    public List<D> search(Map<String, Object> filter, Map<String, Object> operation, DataFetchingEnvironment environment) {
        if (filter.isEmpty()) {
            List<E> entityList = crudService.findAll();
            return !entityList.isEmpty() ? modelMapper.entityToDto(entityList) : List.of();
        }

        environment.getSelectionSet().getFields().forEach(x -> Fields.add(x.getName()));
        System.out.println("Search 2");
        System.out.println(Fields);
        Specification<E> specification = JpaSpecificationBuilder.parseFilterExpression(filter, entityClass, getFormattedFields(Fields));
        List<E> entityList = crudService.findAll(specification);
        List<D> dtos = !entityList.isEmpty() ? modelMapper.entityToDto(entityList) : List.of();
        return dtos;
    }

    @Override
    public String count(Map<String, Object> filter, Map<String, Object> operation) {
        List<Object> things = TypedQueryBuilder.parseFilterExpression(filter, operation, entityClass, null, Operation.COUNT);
        System.out.println("things : " + things);
        System.out.println("things length : " + things.size());

        for (Object result : things) {
            if (result instanceof Long)
                return result.toString();
        }

        return "-1";
    }

    private static List<String> getFormattedFields(List<String> fields) {
        List<String> formattedFields = new ArrayList<>();

        fields.forEach(x -> {
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
            } else {
                if (!x.contains("_results__count___"))
                    if (!formattedFields.contains(x)) {
                        formattedFields.add(x);
                    }
            }
        });

        return formattedFields;
    }
}
