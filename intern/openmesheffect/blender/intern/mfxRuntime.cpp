/**
 * Open Mesh Effect modifier for Blender
 * Copyright (C) 2019 - 2020 Elie Michel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

/** \file
 * \ingroup openmesheffect
 */

#include "MEM_guardedalloc.h"

#include "mfxCallbacks.h"
#include "mfxRuntime.h"
#include "mfxConvert.h"
#include "mfxPluginRegistryPool.h"
#include <mfxHost/mesheffect>
#include <mfxHost/messages>
#include "ofxExtras.h"

#include "DNA_mesh_types.h" // Mesh
#include "DNA_meshdata_types.h" // MVert

#include "BKE_mesh.h" // BKE_mesh_new_nomain
#include "BKE_main.h" // BKE_main_blendfile_path_from_global

#include "BLI_math_vector.h"
#include "BLI_string.h"
#include "BLI_path_util.h"

// ----------------------------------------------------------------------------
// Public

OpenMeshEffectRuntime::OpenMeshEffectRuntime()
{
  plugin_path[0] = '\0';
  m_is_plugin_valid = false;
  effect_index = 0;
  ofx_host = nullptr;
  effect_desc = nullptr;
  effect_instance = nullptr;
  registry = nullptr;
}

OpenMeshEffectRuntime::~OpenMeshEffectRuntime()
{
  reset_plugin_path();

  if (nullptr != this->ofx_host) {
    releaseGlobalHost();
    this->ofx_host = nullptr;
  }
}

void OpenMeshEffectRuntime::set_plugin_path(const char *plugin_path)
{
  if (0 == strcmp(this->plugin_path, plugin_path)) {
    return;
  }

  reset_plugin_path();

  strncpy(this->plugin_path, plugin_path, sizeof(this->plugin_path));

  if (0 == strcmp(this->plugin_path, "")) {
    return;
  }

  printf("Loading OFX plugin %s\n", this->plugin_path);
  
  char abs_path[FILE_MAX];
  normalize_plugin_path(this->plugin_path, abs_path);

  this->registry = get_registry(abs_path);
  m_is_plugin_valid = this->registry != NULL;
}

void OpenMeshEffectRuntime::set_effect_index(int effect_index)
{
  if (this->effect_index == effect_index) {
    return;
  }
  
  if (-1 != this->effect_index) {
    free_effect_instance();
  }

  if (is_plugin_valid()) {
    this->effect_index = min_ii(max_ii(-1, effect_index), this->registry->num_plugins - 1);
  } else {
    this->effect_index = -1;
  }

  if (-1 != this->effect_index) {
    ensure_effect_instance();
  }
}

void OpenMeshEffectRuntime::get_parameters_from_rna(OpenMeshEffectModifierData *fxmd)
{
  OfxParamHandle *parameters = this->effect_instance->parameters.parameters;
  for (int i = 0 ; i < fxmd->num_parameters ; ++i) {
    copy_parameter_value_from_rna(parameters[i], fxmd->parameter_info + i);
  }
}

void OpenMeshEffectRuntime::set_message_in_rna(OpenMeshEffectModifierData *fxmd)
{
  if (NULL == this->effect_instance) {
    return;
  }

  OfxMessageType type = this->effect_instance->messageType;

  if (type != OFX_MESSAGE_INVALID) {
    BLI_strncpy(fxmd->message, this->effect_instance->message, 1024);
    fxmd->message[1023] = '\0';
  }

  if (type == OFX_MESSAGE_ERROR || type == OFX_MESSAGE_FATAL) {
    modifier_setError(&fxmd->modifier, this->effect_instance->message);
  }
}

bool OpenMeshEffectRuntime::ensure_effect_instance()
{

  if (false == is_plugin_valid()) {
    return false;
  }

  if (-1 == this->effect_index) {
    printf("No selected plug-in effect\n");
    return false;
  }

  ensure_host();

  OfxPlugin *plugin = this->registry->plugins[this->effect_index];

  if (NULL == this->effect_desc) {
    // Load plugin if not already loaded
    OfxPluginStatus *pStatus = &this->registry->status[this->effect_index];
    if (OfxPluginStatNotLoaded == *pStatus) {
      if (ofxhost_load_plugin(this->ofx_host, plugin)) {
        *pStatus = OfxPluginStatOK;
      }
      else {
        printf("Error while loading plugin!\n");
        *pStatus = OfxPluginStatError;
        return false;
      }
    }

    ofxhost_get_descriptor(this->ofx_host, plugin, &this->effect_desc);
  }

  if (NULL == this->effect_instance) {
    ofxhost_create_instance(plugin, this->effect_desc, &this->effect_instance);
  }

  return true;
}

bool OpenMeshEffectRuntime::is_plugin_valid() const
{
  return m_is_plugin_valid;
}

void OpenMeshEffectRuntime::save_rna_parameter_values(OpenMeshEffectModifierData *fxmd)
{
  m_saved_parameter_values.clear();
  for (int i = 0; i < fxmd->num_parameters; ++i) {
    OpenMeshEffectParameterInfo* rna = fxmd->parameter_info + i;
    std::string key = std::string(rna->name);
    copy_parameter_value_from_rna(&m_saved_parameter_values[key], rna);
  }
}

void OpenMeshEffectRuntime::try_restore_rna_parameter_values(OpenMeshEffectModifierData *fxmd)
{
  for (int i = 0; i < fxmd->num_parameters; ++i) {
    OpenMeshEffectParameterInfo *rna = fxmd->parameter_info + i;
    std::string key = std::string(rna->name);
    if (m_saved_parameter_values.count(key)) {
      copy_parameter_value_to_rna(rna, &m_saved_parameter_values[key]);
    }
  }
}

Mesh * OpenMeshEffectRuntime::cook(OpenMeshEffectModifierData *fxmd, Mesh *mesh)
{
  if (false == this->ensure_effect_instance()) {
    printf("failed to get effect instance\n");
    return NULL;
  }

  OfxHost *ofxHost = this->ofx_host;
  OfxMeshEffectSuiteV1 *meshEffectSuite = (OfxMeshEffectSuiteV1 *)ofxHost->fetchSuite(
      ofxHost->host, kOfxMeshEffectSuite, 1);
  OfxPropertySuiteV1 *propertySuite = (OfxPropertySuiteV1 *)ofxHost->fetchSuite(
      ofxHost->host, kOfxPropertySuite, 1);

  OfxMeshInputHandle input, output;
  meshEffectSuite->inputGetHandle(this->effect_instance, kOfxMeshMainInput, &input, NULL);
  meshEffectSuite->inputGetHandle(this->effect_instance, kOfxMeshMainOutput, &output, NULL);

  // Get parameters
  this->get_parameters_from_rna(fxmd);

  // Test if we can skip cooking
  OfxPlugin *plugin = this->registry->plugins[this->effect_index];
  bool shouldCook = true;
  ofxhost_is_identity(plugin, this->effect_instance, &shouldCook);

  if (false == shouldCook) {
    printf("effect is identity, skipping cooking\n");
    return mesh;
  }

  // Set input mesh data binding, used by before/after callbacks
  MeshInternalData input_data;
  input_data.is_input = true;
  input_data.blender_mesh = mesh;
  input_data.source_mesh = NULL;
  propertySuite->propSetPointer(
      &input->mesh.properties, kOfxMeshPropInternalData, 0, (void *)&input_data);

  // Set output mesh data binding, used by before/after callbacks
  MeshInternalData output_data;
  output_data.is_input = false;
  output_data.blender_mesh = NULL;
  output_data.source_mesh = mesh;
  propertySuite->propSetPointer(
      &output->mesh.properties, kOfxMeshPropInternalData, 0, (void *)&output_data);

  ofxhost_cook(plugin, this->effect_instance);

  // Free mesh on Blender side
  if (NULL != output_data.blender_mesh && output_data.blender_mesh != output_data.source_mesh) {
    BKE_mesh_free(output_data.source_mesh);
  }

  this->set_message_in_rna(fxmd);

  return output_data.blender_mesh;
}

void OpenMeshEffectRuntime::reload_effect_info(OpenMeshEffectModifierData *fxmd)
{
  // Free previous info
  if (NULL != fxmd->effect_info) {
    MEM_freeN(fxmd->effect_info);
    fxmd->effect_info = NULL;
    fxmd->num_effects = 0;
  }

  if (false == this->is_plugin_valid()) {
    return;
  }

  fxmd->num_effects = this->registry->num_plugins;
  fxmd->effect_info = (OpenMeshEffectEffectInfo *)MEM_calloc_arrayN(
      sizeof(OpenMeshEffectEffectInfo), fxmd->num_effects, "mfx effect info");

  for (int i = 0; i < fxmd->num_effects; ++i) {
    // Get asset name
    const char *name = this->registry->plugins[i]->pluginIdentifier;
    printf("Loading %s to RNA\n", name);
    strncpy(fxmd->effect_info[i].name, name, sizeof(fxmd->effect_info[i].name));
  }
}

void OpenMeshEffectRuntime::reload_parameter_info(OpenMeshEffectModifierData *fxmd)
{

  // Reset parameter DNA
  if (NULL != fxmd->parameter_info) {
    save_rna_parameter_values(fxmd);
    MEM_freeN(fxmd->parameter_info);
    fxmd->parameter_info = NULL;
    fxmd->num_parameters = 0;
  }

  if (NULL == this->effect_desc) {
    printf("==/ mfx_Modifier_on_asset_changed\n");
    return;
  }

  OfxParamSetHandle parameters = &this->effect_desc->parameters;

  fxmd->num_parameters = parameters->num_parameters;
  fxmd->parameter_info = (OpenMeshEffectParameterInfo *)MEM_calloc_arrayN(
      sizeof(OpenMeshEffectParameterInfo), fxmd->num_parameters, "openmesheffect parameter info");

  for (int i = 0; i < fxmd->num_parameters; ++i) {
    const OfxPropertySetStruct & props = parameters->parameters[i]->properties;
    int prop_idx = props.find_property(kOfxParamPropScriptName);
    const char *system_name = prop_idx != -1 ? props.properties[prop_idx]->value->as_const_char :
                                               parameters->parameters[i]->name;
    strncpy(fxmd->parameter_info[i].name, system_name, sizeof(fxmd->parameter_info[i].name));
    strncpy(fxmd->parameter_info[i].label,
            parameters->parameters[i]->name,
            sizeof(fxmd->parameter_info[i].label));
    fxmd->parameter_info[i].type = parameters->parameters[i]->type;

    int default_idx = props.find_property(kOfxParamPropDefault);
    if (default_idx > -1) {
      copy_parameter_value_to_rna(&fxmd->parameter_info[i], props.properties[default_idx]);
    }
  }

  try_restore_rna_parameter_values(fxmd);
}

// ----------------------------------------------------------------------------
// Private static

void OpenMeshEffectRuntime::normalize_plugin_path(char *path, char *out_path)
{
  BLI_strncpy(out_path, path, FILE_MAX);
  const char *base_path =
      BKE_main_blendfile_path_from_global();  // TODO: How to get a bMain object here to avoid
                                              // "from_global()"?
  if (NULL != base_path) {
    BLI_path_abs(out_path, base_path);
  }
}

// ----------------------------------------------------------------------------
// Private

void OpenMeshEffectRuntime::free_effect_instance()
{
  if (is_plugin_valid() && -1 != this->effect_index) {
    OfxPlugin *plugin = this->registry->plugins[this->effect_index];
    OfxPluginStatus status = this->registry->status[this->effect_index];

    printf("runtime_free_effect_instance: plugin = %p, registry.plugins = %p, rd = %p\n",
           plugin,
           this->registry,
           this);

    if (NULL != this->effect_instance) {
      ofxhost_destroy_instance(plugin, this->effect_instance);
      this->effect_instance = NULL;
    }
    if (NULL != this->effect_desc) {
      ofxhost_release_descriptor(this->effect_desc);
      this->effect_desc = NULL;
    }
    if (OfxPluginStatOK == status) {
      // TODO: loop over all plugins?
      ofxhost_unload_plugin(plugin);
      this->registry->status[this->effect_index] = OfxPluginStatNotLoaded;
    }

    this->effect_index = -1;
  }
}

void OpenMeshEffectRuntime::ensure_host()
{
  if (NULL == this->ofx_host) {
    this->ofx_host = getGlobalHost();

    // Configure host
    OfxPropertySuiteV1 *propertySuite = (OfxPropertySuiteV1 *)this->ofx_host->fetchSuite(
        this->ofx_host->host, kOfxPropertySuite, 1);
    // Set custom callbacks
    propertySuite->propSetPointer(
        this->ofx_host->host, kOfxHostPropBeforeMeshGetCb, 0, (void *)before_mesh_get);
    propertySuite->propSetPointer(
        this->ofx_host->host, kOfxHostPropBeforeMeshReleaseCb, 0, (void *)before_mesh_release);
  }
}

void OpenMeshEffectRuntime::reset_plugin_path()
{
  if (is_plugin_valid()) {
    printf("Unloading OFX plugin %s\n", this->plugin_path);
    free_effect_instance();

    char abs_path[FILE_MAX];
    normalize_plugin_path(this->plugin_path, abs_path);
    release_registry(abs_path);
    m_is_plugin_valid = false;
  }
  this->plugin_path[0] = '\0';
  this->effect_index = -1;
}
