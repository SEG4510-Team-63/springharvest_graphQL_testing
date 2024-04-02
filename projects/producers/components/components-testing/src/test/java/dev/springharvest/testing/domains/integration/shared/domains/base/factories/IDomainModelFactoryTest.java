package dev.springharvest.testing.domains.integration.shared.domains.base.factories;

import dev.springharvest.shared.domains.DomainModel;
import org.assertj.core.api.SoftAssertions;
import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

public class IDomainModelFactoryTest {


    IDomainModelFactory<DomainModel> domainModelFactory = new IDomainModelFactory<>() {
        @Override
        public DomainModel buildValidDto() {
            DomainModel validModel = new DomainModel() {
                @Override
                public boolean isEmpty() {
                    return false;
                }
            };

            return validModel;
        }

        @Override
        public DomainModel buildValidUpdatedDto(DomainModel dto) {

            DomainModel updatedModel = new DomainModel() {
                @Override
                public boolean isEmpty() {
                    return false;
                }
            };

            return updatedModel;
        }

        @Override
        public DomainModel buildInvalidDto() {
            DomainModel invalidModel = new DomainModel() {
                @Override
                public boolean isEmpty() {
                    return false;
                }
            };

            return invalidModel;
        }
    };



    @Test
    void shouldSoftlyAssertNonNullObjects() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, "actual", "actual");
        softly.assertAll();
    }
    @Test
    void shouldAssertEqualStrings() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, "testString", "testString");
        softly.assertAll(); // Should pass as both strings are equal
    }

    @Test
    void shouldAssertEqualStringsWithCaseInsensitivity() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, "testString", "TestString");
        softly.assertAll(); // Should pass as StringUtils.capitalizeFirstLetters would make both strings equal
    }

    @Test
    void shouldAssertDifferentStrings() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, "testString", "anotherString");
        softly.assertAll(); // Should fail as the strings are different
    }

    @Test
    void shouldNotAssertWhenActualIsNull() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, null, "notNull");
        softly.assertAll(); // Should pass as the method does not assert when actual is null
    }

    @Test
    void shouldNotAssertWhenExpectedIsNull() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, "notNull", null);
        softly.assertAll();
    }

    @Test
    void shouldNotAssertWhenBothAreNull() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, (DomainModel) null, (DomainModel) null);
        softly.assertAll();
    }

    @Test
    void shouldAssertNonStringObjects() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, 123, 123);
        softly.assertAll(); // Should pass as the integers are equal
    }

    @Test
    void shouldAssertDifferentNonStringObjects() {
        SoftAssertions softly = new SoftAssertions();
        domainModelFactory.softlyAssert(softly, 123, 456);
        softly.assertAll(); // Should fail as the integers are different
    }


}
