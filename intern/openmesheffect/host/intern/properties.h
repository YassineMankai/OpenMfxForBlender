/*
 * Copyright 2019 Elie Michel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/** \file
 * \ingroup openmesheffect
 *
 */

#ifndef __MFX_PROPERTIES_H__
#define __MFX_PROPERTIES_H__

#include <stdbool.h>

union OfxPropertyValueStruct {
    void *as_pointer;
    const char *as_const_char;
    char *as_char;
    double as_double;
    int as_int;
};

struct OfxPropertyStruct {
 public:
  OfxPropertyStruct();
  ~OfxPropertyStruct();

  // Disable copy, we handle it explicitely
  OfxPropertyStruct(const OfxPropertyStruct &) = delete;
  OfxPropertyStruct &operator=(const OfxPropertyStruct &) = delete;


  void deep_copy_from(const OfxPropertyStruct &other);

 public:
  const char *name;
  OfxPropertyValueStruct value[4];
};

typedef enum PropertyType {
	PROP_TYPE_POINTER,
	PROP_TYPE_STRING,
	PROP_TYPE_DOUBLE,
	PROP_TYPE_INT,
} PropertyType;

// TODO: use kOfxPropType instead
typedef enum PropertySetContext {
    PROP_CTX_HOST, // kOfxTypeMeshEffectHost
    PROP_CTX_MESH_EFFECT, // kOfxTypeMeshEffect, kOfxTypeMeshEffectInstance
    PROP_CTX_INPUT, // kOfxTypeMeshEffectInput
    PROP_CTX_MESH, // kOfxTypeMesh
    PROP_CTX_PARAM, // kOfxTypeParameter
    PROP_CTX_ATTRIB,
    PROP_CTX_OTHER,
    // kOfxTypeParameterInstance
} PropertySetContext;

// // OfxPropertySetStruct

struct OfxPropertySetStruct {
 public:
  OfxPropertySetStruct();
  ~OfxPropertySetStruct();

  // Disable copy, we handle it explicitely
  OfxPropertySetStruct(const OfxPropertySetStruct &) = delete;
  OfxPropertySetStruct &operator=(const OfxPropertySetStruct &) = delete;

  int find_property(const char *property) const;
  void append_properties(int count);
  void remove_property(int index);
  int ensure_property(const char *property);

  void deep_copy_from(const OfxPropertySetStruct &other);

 public:
  static bool check_property_context(PropertySetContext context,
                                     PropertyType type,
                                     const char *property);

 public:
  PropertySetContext context; // TODO: use this rather than generic property set objects
  int num_properties;
  OfxPropertyStruct **properties;
};


// // Property Suite Entry Points

#include "ofxProperty.h"

extern const OfxPropertySuiteV1 gPropertySuiteV1;

// See ofxProperty.h for docstrings

OfxStatus propSetPointer(OfxPropertySetHandle properties, const char *property, int index, void *value);
OfxStatus propSetString(OfxPropertySetHandle properties, const char *property, int index, const char *value);
OfxStatus propSetDouble(OfxPropertySetHandle properties, const char *property, int index, double value);
OfxStatus propSetInt(OfxPropertySetHandle properties, const char *property, int index, int value);
OfxStatus propSetPointerN(OfxPropertySetHandle properties, const char *property, int count, void *const*value);
OfxStatus propSetStringN(OfxPropertySetHandle properties, const char *property, int count, const char *const*value);
OfxStatus propSetDoubleN(OfxPropertySetHandle properties, const char *property, int count, const double *value);
OfxStatus propSetIntN(OfxPropertySetHandle properties, const char *property, int count, const int *value);
OfxStatus propGetPointer(OfxPropertySetHandle properties, const char *property, int index, void **value);
OfxStatus propGetString(OfxPropertySetHandle properties, const char *property, int index, char **value);
OfxStatus propGetDouble(OfxPropertySetHandle properties, const char *property, int index, double *value);
OfxStatus propGetInt(OfxPropertySetHandle properties, const char *property, int index, int *value);
OfxStatus propGetPointerN(OfxPropertySetHandle properties, const char *property, int count, void **value);
OfxStatus propGetStringN(OfxPropertySetHandle properties, const char *property, int count, char **value);
OfxStatus propGetDoubleN(OfxPropertySetHandle properties, const char *property, int count, double *value);
OfxStatus propGetIntN(OfxPropertySetHandle properties, const char *property, int count, int *value);
OfxStatus propReset(OfxPropertySetHandle properties, const char *property);
OfxStatus propGetDimension(OfxPropertySetHandle properties, const char *property, int *count);

#endif // __MFX_PROPERTIES_H__
