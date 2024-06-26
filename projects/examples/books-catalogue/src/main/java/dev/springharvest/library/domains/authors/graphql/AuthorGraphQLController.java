package dev.springharvest.library.domains.authors.graphql;

import dev.springharvest.expressions.client.ExpressionFormat;
import dev.springharvest.expressions.client.FilterExpression;
import dev.springharvest.library.domains.authors.constants.AuthorMutationInput;
import dev.springharvest.library.domains.authors.constants.AuthorSearchInput;
import dev.springharvest.library.domains.authors.models.dtos.AuthorDTO;
import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import dev.springharvest.library.domains.authors.services.AuthorCrudService;

import java.time.Instant;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

import dev.springharvest.library.domains.books.models.dtos.BookDTO;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import dev.springharvest.shared.domains.embeddables.traces.trace.models.entities.TraceDataEntity;
import dev.springharvest.shared.domains.embeddables.traces.users.models.entities.TraceUsersEntity;
import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.MutationMapping;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;
import org.testcontainers.shaded.org.checkerframework.checker.units.qual.A;

@Controller
public class AuthorGraphQLController {

  private final AuthorCrudService baseService;
  protected IBaseModelMapper<AuthorDTO, AuthorEntity, UUID> modelMapper;

  @Autowired
  protected AuthorGraphQLController(AuthorCrudService baseService, IBaseModelMapper<AuthorDTO, AuthorEntity, UUID> modelMapper) {
    this.baseService = baseService;
    this.modelMapper = modelMapper;
  }

  @QueryMapping
  List<AuthorDTO> authors(@Argument @NotNull AuthorSearchInput input) {
    //TODO: Fix
    PageRequest authorsToFind = PageRequest.of(input.page(), input.size(), input.sortDirection() == "A" ? Sort.by(input.sortOrder()).ascending() : Sort.by(input.sortOrder()).descending());
    Page<AuthorEntity> authorsPage;

    if (StringUtils.isEmpty(input.name())) {
      authorsPage = baseService.findAll(authorsToFind);
    } else {
      authorsPage = baseService.findByNameContaining(input.name(), authorsToFind);
    }

    Page<AuthorDTO> dtos = authorsPage.hasContent() ? modelMapper.pagedEntityToPagedDto(authorsPage) : Page.empty(authorsToFind);
    return dtos.getContent();
  }

  @QueryMapping
  Optional<AuthorEntity> authorById(@Argument UUID id) {
    return baseService.findById(id);
  }

  @MutationMapping
  AuthorDTO authorMutation(@Argument @NotNull UUID id, @Argument @NotNull AuthorMutationInput input)
  {
    var op = baseService.findById(id);
    if (op.isPresent())
    {
      var author = op.get();
      author.setName(input.name());
      author.getTraceData().getTraceDates().setDateUpdated(LocalDate.now());
      author.getTraceData().getTraceUsers().setUpdatedBy(input.updatedBy());
      baseService.update(author);
      return modelMapper.entityToDto(author);
    }
    else {
      throw new EntityNotFoundException(String.format("No entity found with id: %s", id));
    }
  }
}
