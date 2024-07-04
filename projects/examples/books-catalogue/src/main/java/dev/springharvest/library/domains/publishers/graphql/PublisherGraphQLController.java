package dev.springharvest.library.domains.publishers.graphql;

import dev.springharvest.crud.domains.base.graphql.AbstractGraphQLCrudController;
import dev.springharvest.library.domains.publishers.models.dtos.PublisherDTO;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.library.domains.publishers.services.PetSpecificationCrudService;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import dev.springharvest.shared.constants.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

@Controller
public class PublisherGraphQLController extends AbstractGraphQLCrudController<PublisherDTO, PublisherEntity, UUID> {

  @Autowired
  protected PublisherGraphQLController(PetSpecificationCrudService baseService, IBaseModelMapper<PublisherDTO, PublisherEntity, UUID> modelMapper) {
      super(modelMapper, baseService, PublisherEntity.class);
  }

    @QueryMapping
    public List<PublisherDTO> searchPublishers(@Argument @NotNull Map<String, Object> filter, @Argument DataPaging paging) {
      return paging != null ? search(filter, paging) : search(filter);
    }
}
