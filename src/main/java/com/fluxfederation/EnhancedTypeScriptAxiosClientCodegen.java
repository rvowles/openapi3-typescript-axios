package com.fluxfederation;

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.AbstractTypeScriptClientCodegen;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class EnhancedTypeScriptAxiosClientCodegen extends AbstractTypeScriptClientCodegen {
  public static final String NPM_REPOSITORY = "npmRepository";
  public static final String WITH_INTERFACES = "withInterfaces";
  public static final String SEPARATE_MODELS_AND_API = "withSeparateModelsAndApi";
  public static final String WITHOUT_PREFIX_ENUMS = "withoutPrefixEnums";
  public static final String USE_SINGLE_REQUEST_PARAMETER = "useSingleRequestParameter";
  public static final String USE_ENHANCED_SERIALIZER = "useEnhancedSerializer";
  public static final String USE_COALESCE_RETURN_TYPES = "useCoalesceReturnTypes";

  public static final String TEMPLATE_FOLDER = "enhanced-axios-ts";

  protected String npmRepository = null;

  private String tsModelPackage = "";

  public EnhancedTypeScriptAxiosClientCodegen() {
    super();

    modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

    // clear import mapping (from default generator) as TS does not use it
    // at the moment
    importMapping.clear();

    outputFolder = "generated-code/typescript-axios";
    embeddedTemplateDir = templateDir = TEMPLATE_FOLDER;
    
    typeMapping.put("DateTime", "Date");
    typeMapping.put("date", "Date");
    typeMapping.put("AnyType", "any");

    this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url of your private npmRepo in the package.json"));
    this.cliOptions.add(new CliOption(WITH_INTERFACES,
      "Setting this property to true will generate interfaces next to the default class implementations.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    this.cliOptions.add(new CliOption(SEPARATE_MODELS_AND_API, "Put the model and api in separate folders and in separate classes", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    this.cliOptions.add(new CliOption(WITHOUT_PREFIX_ENUMS, "Don't prefix enum names with class names", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    this.cliOptions.add(new CliOption(USE_SINGLE_REQUEST_PARAMETER, "Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.", SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
    this.cliOptions.add(new CliOption(USE_ENHANCED_SERIALIZER, "Setting this will ensure Axios complies with " +
      "serialization and validation rules and it generates classes to match interfaces"));
    this.cliOptions.add(new CliOption(USE_COALESCE_RETURN_TYPES, "Make a function return all of the  types it " +
      "actually returns wrapped in an AxiosResponse."));
  }

  @Override
  public String getName() {
    return "typescript-axios-enhanced";
  }

  @Override
  public String getHelp() {
    return "Generates a TypeScript client library using axios.";
  }

  public String getNpmRepository() {
    return npmRepository;
  }

  public void setNpmRepository(String npmRepository) {
    this.npmRepository = npmRepository;
  }

  private static String getRelativeToRoot(String path) {
    StringBuilder sb = new StringBuilder();
    int slashCount = path.split("/").length;
    if (slashCount == 0) {
      sb.append("./");
    } else {
      for (int i = 0; i < slashCount; ++i) {
        sb.append("../");
      }
    }
    return sb.toString();
  }

  @Override
  public void processOpts() {
    super.processOpts();
    tsModelPackage = modelPackage.replaceAll("\\.", "/");
    String tsApiPackage = apiPackage.replaceAll("\\.", "/");

    String modelRelativeToRoot = getRelativeToRoot(tsModelPackage);
    String apiRelativeToRoot = getRelativeToRoot(tsApiPackage);

    additionalProperties.put("tsModelPackage", tsModelPackage);
    additionalProperties.put("tsApiPackage", tsApiPackage);
    additionalProperties.put("apiRelativeToRoot", apiRelativeToRoot);
    additionalProperties.put("modelRelativeToRoot", modelRelativeToRoot);

    supportingFiles.add(new SupportingFile("index.mustache", "", "index.ts"));
    supportingFiles.add(new SupportingFile("baseApi.mustache", "", "base.ts"));
    supportingFiles.add(new SupportingFile("api.mustache", "", "api.ts"));
    supportingFiles.add(new SupportingFile("configuration.mustache", "", "configuration.ts"));
    supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
    supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
    supportingFiles.add(new SupportingFile("npmignore", "", ".npmignore"));

    if (additionalProperties.containsKey(SEPARATE_MODELS_AND_API)) {
      boolean separateModelsAndApi = Boolean.parseBoolean(additionalProperties.get(SEPARATE_MODELS_AND_API).toString());
      if (separateModelsAndApi) {
        if (StringUtils.isAnyBlank(modelPackage, apiPackage)) {
          throw new RuntimeException("apiPackage and modelPackage must be defined");
        }
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("apiInner.mustache", ".ts");
        supportingFiles.add(new SupportingFile("modelIndex.mustache", tsModelPackage, "index.ts"));
      }
    }

//    if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
//
//    }

    if (additionalProperties.containsKey(NPM_NAME)) {
      addNpmPackageGeneration();
    }

  }

  private void enhanceDataTarget(String dataType, String dataFormat, Map<String, Object> vendorExtensions) {
    if (dataType != null) {
      if ("string".equals(dataType.toLowerCase())) {
        if (dataFormat == null) {
          vendorExtensions.put("x-ts-string-type", Boolean.TRUE);
          vendorExtensions.put("x-ts-deserialize-type",  "string");
        } else {
          vendorExtensions.put("x-ts-deserialize-type",  dataFormat);
        }
      } else {
        if ("date".equals(dataType.toLowerCase())) {
          vendorExtensions.put("x-ts-deserialize-type",  dataFormat);
        } else {
          vendorExtensions.put("x-ts-deserialize-type", dataType);
        }
      }
    } else {
      vendorExtensions.put("x-ts-deserialize-type", "object");
    }
  }

  @Override
  public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
    objs = super.postProcessOperationsWithModels(objs, allModels);
    Map<String, Object> vals = (Map<String, Object>) objs.getOrDefault("operations", new HashMap<>());
    List<CodegenOperation> operations = (List<CodegenOperation>) vals.getOrDefault("operation", new ArrayList<>());
        /*
            Filter all the operations that are multipart/form-data operations and set the vendor extension flag
            'multipartFormData' for the template to work with.
         */
    operations.stream()
      .filter(op -> op.hasConsumes)
      .filter(op -> op.consumes.stream().anyMatch(opc -> opc.values().stream().anyMatch("multipart/form-data"::equals)))
      .forEach(op -> op.vendorExtensions.putIfAbsent("multipartFormData", true));

    if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
      operations.stream()
        .filter(op -> op.hasProduces)
        .forEach(op -> {
          Set<String> responseTypes = op.responses.stream()
            .map(r -> (r.dataType == null) ? "void" : r.dataType)
            .collect(Collectors.toSet());
          if (responseTypes.isEmpty()) {
            responseTypes = new HashSet<>(Collections.singletonList("void"));
          }
          op.vendorExtensions.put("x-ts-responseTypes", String.join("|", responseTypes));

          op.responses.forEach(bp -> {
            enhanceDataTarget(bp.dataType, null, bp.vendorExtensions);
            bp.vendorExtensions.put("x-ts-is-error", bp.is4xx || bp.is5xx);
          });
        });
      operations.stream()
        .filter(op -> op.hasConsumes)
        .filter(op -> op.bodyParam != null)
        .map(op -> op.bodyParam)
        .forEach(bp -> enhanceDataTarget(bp.dataType, bp.dataFormat, bp.vendorExtensions));
    }
    return objs;
  }

  @Override
  public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
    Map<String, Object> result = super.postProcessAllModels(objs);
    for (Map.Entry<String, Object> entry : result.entrySet()) {
      Map<String, Object> inner = (Map<String, Object>) entry.getValue();
      List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
      for (Map<String, Object> model : models) {
        CodegenModel codegenModel = (CodegenModel) model.get("model");
        model.put("hasAllOf", codegenModel.allOf.size() > 0);
        model.put("hasOneOf", codegenModel.oneOf.size() > 0);
      }
    }
    return result;
  }


  @Override
  protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
    codegenModel.additionalPropertiesType = getTypeDeclaration(ModelUtils.getAdditionalProperties(schema));
    addImport(codegenModel, codegenModel.additionalPropertiesType);
  }

  private void postProcessCodegenProperty(CodegenProperty var) {
    if (var.dataType == null) {
      var.vendorExtensions.put("x-ts-deserialize-type", "object");
    } else {
      if (var.dataType.startsWith("{")) {
        var.vendorExtensions.put("x-ts-record-type",  "Record<string, " + var.items.dataType + ">");
        var.vendorExtensions.put("x-ts-additional-props", var.items.dataType);
      } else if ("string".equals(var.dataType.toLowerCase())) {
        if (var.dataFormat == null) {
          var.vendorExtensions.put("x-ts-deserialize-type",  "string");
        } else {
          var.vendorExtensions.put("x-ts-deserialize-type",  var.dataFormat);
        }
      } else {
        if ("date".equals(var.dataType.toLowerCase())) {
          var.vendorExtensions.put("x-ts-deserialize-type",  var.dataFormat);
        } else {
          var.vendorExtensions.put("x-ts-deserialize-type", var.dataType);
        }
      }
    }
  }

  @Override
  @SuppressWarnings("unchecked")
  public Map<String, Object> postProcessModels(Map<String, Object> objs) {
    List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");

    boolean withoutPrefixEnums = false;
    if (additionalProperties.containsKey(WITHOUT_PREFIX_ENUMS)) {
      withoutPrefixEnums = Boolean.parseBoolean(additionalProperties.get(WITHOUT_PREFIX_ENUMS).toString());
    }

    for (Object _mo  : models) {
      Map<String, Object> mo = (Map<String, Object>) _mo;
      CodegenModel cm = (CodegenModel) mo.get("model");

      // Deduce the model file name in kebab case
      cm.classFilename = cm.classname.replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);


      if (additionalProperties.containsKey(USE_ENHANCED_SERIALIZER)) {
        for (CodegenProperty var : cm.vars) {
          postProcessCodegenProperty(var);
        }
      }

      //processed enum names
      if(!withoutPrefixEnums) {
        cm.imports = new TreeSet(cm.imports);
        // name enum with model name, e.g. StatusEnum => PetStatusEnum
        for (CodegenProperty var : cm.vars) {
          if (Boolean.TRUE.equals(var.isEnum)) {
            var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
            var.enumName = var.enumName.replace(var.enumName, cm.classname + var.enumName);
          }
        }
        if (cm.parent != null) {
          for (CodegenProperty var : cm.allVars) {
            if (Boolean.TRUE.equals(var.isEnum)) {
              var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
              var.enumName = var.enumName.replace(var.enumName, cm.classname + var.enumName);
            }
          }
        }
      }
    }

    // Apply the model file name to the imports as well
    for (Map<String, String> m : (List<Map<String, String>>) objs.get("imports")) {
      String javaImport = m.get("import").substring(modelPackage.length() + 1);
      String tsImport = tsModelPackage + "/" + javaImport;
      m.put("tsImport", tsImport);
      m.put("class", javaImport);
      m.put("filename", javaImport.replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT));
    }
    return objs;
  }

  /**
   * Overriding toRegularExpression() to avoid escapeText() being called,
   * as it would return a broken regular expression if any escaped character / metacharacter were present.
   */
  @Override
  public String toRegularExpression(String pattern) {
    return addRegularExpressionDelimiter(pattern);
  }

  @Override
  public String toModelFilename(String name) {
    return super.toModelFilename(name).replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);
  }

  @Override
  public String toVarName(String name) {
    name = name
      .replaceAll("-", "_")
      .replaceAll("\\$", "__");

    if (name.matches("^[A-Z_]*$")) {
      return name;
    } else {
      name = org.openapitools.codegen.utils.StringUtils.camelize(name, true);
      if (name.matches("^\\d.*")) {
        name = "n" + name;
      }

      if (this.isReservedWord(name)) {
        name = this.escapeReservedWord(name);
      }

      return name.replaceAll("@", "_");
    }
  }

  @Override
  public String toApiFilename(String name) {
    return super.toApiFilename(name).replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(Locale.ROOT);
  }

  private void addNpmPackageGeneration() {

    if (additionalProperties.containsKey(NPM_REPOSITORY)) {
      this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
    }

    //Files for building our lib
    supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
    supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
    supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
  }
}
