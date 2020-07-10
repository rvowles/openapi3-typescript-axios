package com.fluxfederation;

import org.junit.Test;
import org.openapitools.codegen.OpenAPIGenerator;

import java.util.Arrays;

public class SampleTest {
  @Test
  public void runGenerator() {
    String location = getClass().getResource("/sample1.yaml").getFile();
    OpenAPIGenerator.main(Arrays.asList("generate",
      "--input-spec", location,
      "--generator-name", "typescript-axios-enhanced",
      "--additional-properties", "useEnhancedSerializer=true",
      "--output", "target/" + getClass().getSimpleName())
      .toArray(new String[0]));
  }
}
