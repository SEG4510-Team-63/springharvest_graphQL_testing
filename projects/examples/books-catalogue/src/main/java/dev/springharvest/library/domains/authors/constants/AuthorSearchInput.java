package dev.springharvest.library.domains.authors.constants;

public record AuthorSearchInput(String name, int page, int size, String sortOrder, String sortDirection) {
}
