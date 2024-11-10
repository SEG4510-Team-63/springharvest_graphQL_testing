import dev.springharvest.library.domains.authors.models.entities.AuthorEntity;
import dev.springharvest.library.domains.books.models.entities.BookEntity;
import dev.springharvest.library.domains.pet.models.entities.PetEntity;
import dev.springharvest.library.domains.publishers.models.entities.PublisherEntity;
import dev.springharvest.expressions.mappers.GenericEntityMapper;
import jakarta.persistence.Tuple;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.mockito.Mockito;

import java.util.UUID;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

class EntityMapperTest {

    private static final Logger logger = Logger.getLogger(EntityMapperTest.class.getName());

    private GenericEntityMapper<BookEntity> bookMapper;
    private GenericEntityMapper<AuthorEntity> authorMapper;
    private GenericEntityMapper<PublisherEntity> publisherMapper;
    private GenericEntityMapper<PetEntity> petMapper;

    @BeforeEach
    void setUp() {
        bookMapper = new GenericEntityMapper<>();
        authorMapper = new GenericEntityMapper<>();
        publisherMapper = new GenericEntityMapper<>();
        petMapper = new GenericEntityMapper<>();
    }

    @BeforeEach
    void logBeforeTest(TestInfo testInfo) {
        logger.info("Starting test: " + testInfo.getDisplayName());
    }

//    @AfterEach
//    void logAfterTest(TestInfo testInfo) {
//        logger.info("Finished test: " + testInfo.getDisplayName());
//    }

    @Test
    void testMapToBookEntityFromTuple() {
        Tuple tuple = Mockito.mock(Tuple.class);
        UUID bookId = UUID.randomUUID();
        String title = "Test Book Title";
        String genre = "Fiction";
        AuthorEntity author = new AuthorEntity();
        PublisherEntity publisher = new PublisherEntity();

        when(tuple.get("id", UUID.class)).thenReturn(bookId);
        when(tuple.get("title", String.class)).thenReturn(title);
        when(tuple.get("genre", String.class)).thenReturn(genre);
        when(tuple.get("author", AuthorEntity.class)).thenReturn(author);
        when(tuple.get("publisher", PublisherEntity.class)).thenReturn(publisher);

        BookEntity bookEntity = bookMapper.mapToEntity(tuple, BookEntity.class);

        System.out.println("------------------------------Book Entity Mapping Test Start------------------------------");
        System.out.println("Book Publisher; Expected: " + publisher + ", Actual: " + bookEntity.getPublisher());
        System.out.println("Book Author; Expected: " + author + ", Actual: " + bookEntity.getAuthor());
        System.out.println("Book Genre; Expected: " + genre + ", Actual: " + bookEntity.getGenre());
        System.out.println("Book Title; Expected: " + title + ", Actual: " + bookEntity.getTitle());
        System.out.println("Book ID; Expected: " + bookId + ", Actual: " + bookEntity.getId());
        System.out.println("------------------------------Book Entity Mapping Test End------------------------------");

        assertNotNull(bookEntity);
        //assertEquals(bookId, bookEntity.getId());
        assertEquals(title, bookEntity.getTitle());
        assertEquals(genre, bookEntity.getGenre());
        assertEquals(author, bookEntity.getAuthor());
        assertEquals(publisher, bookEntity.getPublisher());
    }

    @Test
    void testMapToAuthorEntityFromTuple() {
        Tuple tuple = Mockito.mock(Tuple.class);
        UUID authorId = UUID.randomUUID();
        String name = "Author Name";
        PetEntity pet = new PetEntity();

        when(tuple.get("id", UUID.class)).thenReturn(authorId);
        when(tuple.get("name", String.class)).thenReturn(name);
        when(tuple.get("pet", PetEntity.class)).thenReturn(pet);

        AuthorEntity authorEntity = authorMapper.mapToEntity(tuple, AuthorEntity.class);

        assertNotNull(authorEntity);
        //assertEquals(authorId, authorEntity.getId());
        assertEquals(name, authorEntity.getName());
        assertEquals(pet, authorEntity.getPet());
    }

    @Test
    void testMapToPublisherEntityFromTuple() {
        Tuple tuple = Mockito.mock(Tuple.class);
        UUID publisherId = UUID.randomUUID();
        String name = "Publisher Name";

        when(tuple.get("id", UUID.class)).thenReturn(publisherId);
        when(tuple.get("name", String.class)).thenReturn(name);

        PublisherEntity publisherEntity = publisherMapper.mapToEntity(tuple, PublisherEntity.class);

        assertNotNull(publisherEntity);
        //assertEquals(publisherId, publisherEntity.getId());
        assertEquals(name, publisherEntity.getName());
    }

    @Test
    void testMapToPetEntityFromTuple() {
        Tuple tuple = Mockito.mock(Tuple.class);
        UUID petId = UUID.randomUUID();
        String name = "Pet Name";

        when(tuple.get("id", UUID.class)).thenReturn(petId);
        when(tuple.get("name", String.class)).thenReturn(name);

        PetEntity petEntity = petMapper.mapToEntity(tuple, PetEntity.class);

        assertNotNull(petEntity);
        //assertEquals(petId, petEntity.getId());
        assertEquals(name, petEntity.getName());
    }
}