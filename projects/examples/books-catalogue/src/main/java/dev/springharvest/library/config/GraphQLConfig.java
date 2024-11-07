package dev.springharvest.library.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Bean;
import org.springframework.graphql.execution.RuntimeWiringConfigurer;
import graphql.scalars.ExtendedScalars;

// This class is used to configure GraphQL in the application
@Configuration
public class GraphQLConfig {

    // This bean is used to configure the runtime wiring for GraphQL
    // Here we are adding a scalar for Date type
    @Bean
    RuntimeWiringConfigurer runtimeWiringConfigurer(){
        return wiringBuilder ->
                wiringBuilder.scalar(ExtendedScalars.Date); // Adding Date scalar
    }
}