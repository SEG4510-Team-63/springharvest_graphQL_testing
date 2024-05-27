package dev.springharvest.library.domains.books.controllers.graphql;

import dev.springharvest.expressions.client.ExpressionFormat;
import dev.springharvest.expressions.client.FilterExpression;
import dev.springharvest.expressions.helpers.JpaSpecificationHelper;
import dev.springharvest.library.domains.books.constants.BookSearchInput;
import dev.springharvest.library.domains.books.models.dtos.BookDTO;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import dev.springharvest.library.domains.books.services.BookCrudService;

import java.util.*;

import dev.springharvest.library.global.DataPaging;
import dev.springharvest.shared.domains.base.mappers.IBaseModelMapper;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;
import org.apache.commons.lang3.StringUtils;

@Controller
public class BookGraphQLController {

  private final BookCrudService baseService;
  protected IBaseModelMapper<BookDTO, BookEntity, UUID> modelMapper;
  private Specification<BookEntity> specification;

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
  List<BookDTO> searchBooks(@Argument @NotNull Map filter, @Argument DataPaging paging) {
    var pageRequest = PageRequest.of(paging.page(), paging.size(), paging.sortDirection() == "A" ? Sort.by(paging.sortOrders()).ascending() : Sort.by(paging.sortOrders()).descending());
    specification = JpaSpecificationHelper.parseFilterExpression(filter);

    Page<BookEntity> booksPage  = baseService.findAll(specification, pageRequest);

    Page<BookDTO> dtos = booksPage.hasContent() ? modelMapper.pagedEntityToPagedDto(booksPage) : Page.empty(pageRequest);
    return dtos.getContent();
  }

  @QueryMapping
  List<BookDTO> findBooksByAuthorName(@Argument @NotNull String name, @Argument Map bookFilter) {
    var authorName = name;

    Specification<BookEntity> joinSpecification = JpaSpecificationHelper.createJoinSpecification("author", "name", authorName);

    if(bookFilter != null)
    {
      Specification<BookEntity> specification = JpaSpecificationHelper.parseFilterExpression(bookFilter);

      if (specification != null) {
        /* Create a join condition between Author and Book*/
        joinSpecification =  specification.and(joinSpecification);
      }
    }

    List<BookEntity> books = baseService.findAll(joinSpecification);
    return modelMapper.entityToDto(books);
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
