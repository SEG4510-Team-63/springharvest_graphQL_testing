package dev.springharvest.library.domains.books.constants;

public record BookSearchInput(String title, int page, int size, String sortOrder, String sortDirection) {
}
