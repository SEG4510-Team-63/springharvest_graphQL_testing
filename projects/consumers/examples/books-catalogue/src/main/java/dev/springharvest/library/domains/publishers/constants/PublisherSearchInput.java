package dev.springharvest.library.domains.publishers.constants;

public record PublisherSearchInput(String name, int page, int size, String sortOrder, String sortDirection) {
}
