package dev.springharvest.library.domains.publishers.graphql;

import dev.springharvest.crud.domains.base.graphql.AbstractGraphQLCrudController;
import dev.springharvest.library.domains.publishers.models.dtos.PublisherDTO;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.library.domains.publishers.services.PetQueryCrudService;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.constants.PageData;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import graphql.schema.DataFetchingEnvironment;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

@Controller
public class PublisherGraphQLController extends AbstractGraphQLCrudController<PublisherEntity, UUID> {

  @Autowired
  protected PublisherGraphQLController(PetQueryCrudService baseService) {
      super(baseService, PublisherEntity.class, UUID.class);
  }

    @QueryMapping
    public PageData<PublisherEntity> searchPublishers(@Argument Map<String, Object> filter, @Argument Map<String, Object> clause, @Argument DataPaging paging, DataFetchingEnvironment environment) {
      return search(filter, clause, paging, environment);
    }

  @QueryMapping
  public long countPublishers(@Argument Map<String, Object> filter, @Argument Map<String, Object> clause, @Argument List<String> fields) {
    return count(filter, clause, fields);
  }
}
