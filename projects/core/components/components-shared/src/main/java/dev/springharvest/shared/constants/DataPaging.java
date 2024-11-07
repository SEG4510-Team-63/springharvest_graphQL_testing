package dev.springharvest.shared.constants;
/**
* This record is used to represent data paging
* It contains various fields to represent different paging conditions
*/
public record DataPaging(
    int page, // Represents the current page number
    int size, // Represents the number of items per page
    String sortOrders, // Represents the order of sorting (e.g., "name,age")
    String sortDirection // Represents the direction of sorting (e.g., "asc" or "desc")
) {}
