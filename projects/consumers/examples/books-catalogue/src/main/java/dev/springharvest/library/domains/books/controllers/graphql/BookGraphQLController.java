package dev.springharvest.library.domains.books.controllers.graphql;

import dev.springharvest.library.domains.books.constants.BookSearchInput;
import dev.springharvest.library.domains.books.models.dtos.BookDTO;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import dev.springharvest.library.domains.books.services.BookCrudService;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;
import org.apache.commons.lang3.StringUtils;

@Controller
public class BookGraphQLController {

  private final BookCrudService baseService;
  protected IBaseModelMapper<BookDTO, BookEntity, UUID> modelMapper;

  @Autowired
  protected BookGraphQLController(IBaseModelMapper<BookDTO, BookEntity, UUID> modelMapper, BookCrudService baseService) {
    this.baseService = baseService;
    this.modelMapper = modelMapper;
  }

  @QueryMapping
  List<BookDTO> books(@Argument @NotNull BookSearchInput input) {
    PageRequest booksToFind = PageRequest.of(input.page(), input.size(), input.sortDirection() == "A" ? Sort.by(input.sortOrder()).ascending() : Sort.by(input.sortOrder()).descending());
    Page<BookEntity> booksPage;

    if (StringUtils.isEmpty(input.title())) {
      booksPage = baseService.findAll(booksToFind);
    } else {
      booksPage = baseService.findByTitleContaining(input.title(), booksToFind);
    }

    Page<BookDTO> dtos = booksPage.hasContent() ? modelMapper.pagedEntityToPagedDto(booksPage) : Page.empty(booksToFind);
    return dtos.getContent();
  }

  @QueryMapping
  Optional<BookEntity> bookById(@Argument @NotNull UUID id) {
    return baseService.findById(id);
  }

  @QueryMapping
  public List<BookEntity> booksByAuthor(@Argument @NotNull UUID id) {
    return baseService.findByAuthorId(id);
  }

  @QueryMapping
  public List<BookEntity> booksByPublisher(@Argument @NotNull UUID id) {
    return baseService.findByPublisherId(id);
  }
}
