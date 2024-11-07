package dev.springharvest.shared.constants;

public enum SortDirections {
    A("A", "ASCENDING"),
    D("D", "DESCENDING");

    private String name;
    private String description;

    SortDirections(String name, String description) {
        this.name = name;
        this.description = description;
    }

}


