package dev.springharvest.library.domains.books.graphql;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

@ExtendWith(SpringExtension.class)
@SpringBootTest
@AutoConfigureMockMvc
class BookGraphQLControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void getBookByIdReturnsBook() throws Exception {
        mockMvc.perform(post("/graphql")
                        .content("{\"query\":\"{ book(id: 1) { id, title, author } }\"}")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data.book.id").value(1))
                .andExpect(jsonPath("$.data.book.title").value("Sample Book"))
                .andExpect(jsonPath("$.data.book.author").value("Sample Author"));
    }

    @Test
    void getBookByIdReturnsNotFoundForInvalidId() throws Exception {
        mockMvc.perform(post("/graphql")
                        .content("{\"query\":\"{ book(id: 999) { id, title, author } }\"}")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data.book").isEmpty());
    }

    @Test
    void searchBooksReturnsResults() throws Exception {
        mockMvc.perform(post("/graphql")
                        .content("{\"query\":\"{ searchBooks(title: \\\"Sample\\\") { id, title, author } }\"}")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data.searchBooks").isArray())
                .andExpect(jsonPath("$.data.searchBooks[0].title").value("Sample Book"));
    }

    @Test
    void searchBooksReturnsEmptyForNoMatches() throws Exception {
        mockMvc.perform(post("/graphql")
                        .content("{\"query\":\"{ searchBooks(title: \\\"NonExistent\\\") { id, title, author } }\"}")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data.searchBooks").isEmpty());
    }

    @Test
    void addBookCreatesNewBook() throws Exception {
        mockMvc.perform(post("/graphql")
                        .content("{\"query\":\"mutation { addBook(title: \\\"New Book\\\", author: \\\"New Author\\\") { id, title, author } }\"}")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data.addBook.title").value("New Book"))
                .andExpect(jsonPath("$.data.addBook.author").value("New Author"));
    }

    @Test
    void addBookFailsForMissingTitle() throws Exception {
        mockMvc.perform(post("/graphql")
                        .content("{\"query\":\"mutation { addBook(author: \\\"New Author\\\") { id, title, author } }\"}")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest());
    }
}
