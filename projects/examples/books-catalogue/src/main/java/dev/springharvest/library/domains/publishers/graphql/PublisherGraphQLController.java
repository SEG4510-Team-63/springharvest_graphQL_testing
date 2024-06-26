package dev.springharvest.library.domains.publishers.graphql;

import dev.springharvest.library.domains.publishers.constants.PublisherSearchInput;
import dev.springharvest.library.domains.publishers.models.dtos.PublisherDTO;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.library.domains.publishers.services.PublisherCrudService;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

@Controller
public class PublisherGraphQLController {

  private final PublisherCrudService baseService;
  protected IBaseModelMapper<PublisherDTO, PublisherEntity, UUID> modelMapper;

  @Autowired
  protected PublisherGraphQLController(PublisherCrudService baseService, IBaseModelMapper<PublisherDTO, PublisherEntity, UUID> modelMapper) {
    this.baseService = baseService;
    this.modelMapper = modelMapper;
  }

  @QueryMapping()
  List<PublisherDTO> publishers(@Argument @NotNull PublisherSearchInput input) {
    PageRequest publishersToFind = PageRequest.of(input.page(), input.size(), input.sortDirection() == "A" ? Sort.by(input.sortOrder()).ascending() : Sort.by(input.sortOrder()).descending());
    Page<PublisherEntity> publishersPage;

    if (StringUtils.isEmpty(input.name())) {
      publishersPage = baseService.findAll(publishersToFind);
    } else {
      publishersPage = baseService.findByNameContaining(input.name(), publishersToFind);
    }

    Page<PublisherDTO> dtos = publishersPage.hasContent() ? modelMapper.pagedEntityToPagedDto(publishersPage) : Page.empty(publishersToFind);
    return dtos.getContent();
  }

  @QueryMapping()
  Optional<PublisherEntity> publisherById(@Argument UUID id) {
    return baseService.findById(id);
  }

}
