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

/**
 * This is the GraphQL controller for the Book domain.
 * It provides query operations for books.
 *
 */
@Controller
public class BookGraphQLController {

  private final BookCrudService baseService;
  protected IBaseModelMapper<BookDTO, BookEntity, UUID> modelMapper;
  private Specification<BookEntity> specification;

  /**
   * Constructs a new BookGraphQLController with the given service and model mapper.
   *
   * @param modelMapper the model mapper to use for converting between DTOs and entities
   * @param baseService the service to use for book operations
   */
  @Autowired
  protected BookGraphQLController(IBaseModelMapper<BookDTO, BookEntity, UUID> modelMapper, BookCrudService baseService) {
    this.baseService = baseService;
    this.modelMapper = modelMapper;
  }

  /**
   * Returns a list of books based on the given search input.
   *
   * @param input the search input
   * @return a list of books
   */
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

  /**
   * Returns a list of books based on the given filter and paging information.
   *
   * @param filter the filter to apply
   * @param paging the paging information
   * @return a list of books
   */
  @QueryMapping
  List<BookDTO> searchBooks(@Argument @NotNull Map filter, @Argument DataPaging paging) {
    var pageRequest = PageRequest.of(paging.page(), paging.size(), paging.sortDirection() == "A" ? Sort.by(paging.sortOrders()).ascending() : Sort.by(paging.sortOrders()).descending());
    specification = JpaSpecificationHelper.parseFilterExpression(filter);

    Page<BookEntity> booksPage  = baseService.findAll(specification, pageRequest);

    Page<BookDTO> dtos = booksPage.hasContent() ? modelMapper.pagedEntityToPagedDto(booksPage) : Page.empty(pageRequest);
    return dtos.getContent();
  }

  /**
   * Returns a list of books by the author's name and book filter.
   *
   * @param name the author's name
   * @param bookFilter the book filter to apply
   * @return a list of books
   */
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

  /**
   * Returns a book by its ID.
   *
   * @param id the ID of the book
   * @return the book, or an empty Optional if no book was found with the given ID
   */
  @QueryMapping
  Optional<BookEntity> bookById(@Argument @NotNull UUID id) {
    return baseService.findById(id);
  }

  /**
   * Returns a list of books by the author's ID.
   *
   * @param id the ID of the author
   * @return a list of books
   */
  @QueryMapping
  public List<BookEntity> booksByAuthor(@Argument @NotNull UUID id) {
    return baseService.findByAuthorId(id);
  }

  /**
   * Returns a list of books by the publisher's ID.
   *
   * @param id the ID of the publisher
   * @return a list of books
   */
  @QueryMapping
  public List<BookEntity> booksByPublisher(@Argument @NotNull UUID id) {
    return baseService.findByPublisherId(id);
  }
}

