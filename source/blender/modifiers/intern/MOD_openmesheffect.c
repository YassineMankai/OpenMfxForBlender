/*
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * The Original Code is Copyright (C) 2005 Blender Foundation.
 * All rights reserved.
 */

/** \file
 * \ingroup modifiers
 */

#include "MEM_guardedalloc.h"

#include "BLI_utildefines.h"

#include "DNA_mesh_types.h"
#include "BKE_context.h"
#include "BKE_screen.h"
#include "BKE_modifier.h"

#include "UI_interface.h"
#include "UI_resources.h"

#include "RNA_access.h"

#include "DNA_mesh_types.h"
#include "DNA_meshdata_types.h"
#include "DNA_screen_types.h"

#include "MOD_modifiertypes.h"
#include "MOD_ui_common.h"
#include "MOD_util.h"

#include "BLO_read_write.h"

#include "mfxModifier.h"

// Modifier API

static Mesh *modifyMesh(ModifierData *md,
                           const ModifierEvalContext *ctx,
                           Mesh *mesh)
{
  printf("OpenMeshEffectModifier: modifyMesh.\n");
  OpenMeshEffectModifierData *fxmd = (OpenMeshEffectModifierData *)md;
  return mfx_Modifier_do(fxmd, mesh);
}

static void initData(struct ModifierData *md)
{
  printf("OpenMeshEffectModifier: initData.\n");
  OpenMeshEffectModifierData *fxmd = (OpenMeshEffectModifierData *)md;
  fxmd->effect_index = -1;
  fxmd->num_effects = 0;
  fxmd->effect_info = NULL;
  fxmd->num_parameters = 0;
  fxmd->parameter_info = NULL;
  fxmd->message[0] = '\0';
}

static void copyData(const ModifierData *md, ModifierData *target, const int flag)
{
  printf("OpenMeshEffectModifier: copyData.\n");
  OpenMeshEffectModifierData *fxmd = (OpenMeshEffectModifierData *)md;
  OpenMeshEffectModifierData *tfxmd = (OpenMeshEffectModifierData *)target;

  // A bit dirty to modify the copy source, but otherwise this would have to be in readfile.c,
  // which I don't want to depend on mfxModifier.h
  mfx_Modifier_reload_effect_info(fxmd);

  BKE_modifier_copydata_generic(md, target, flag);

  mfx_Modifier_copydata(fxmd, tfxmd);
}

static void requiredDataMask(Object *UNUSED(ob),
                             ModifierData *UNUSED(md),
                             CustomData_MeshMasks *r_cddata_masks)
{
  /* ask for extra attibutes.
     maybe there could be a mechanism in OpenMeshEffect to have a plugin explicitely
     ask for input attributes, so that we can avoid feeding all of them to addons
     that are not using it. */
  r_cddata_masks->lmask |= CD_MLOOPUV;
  r_cddata_masks->lmask |= CD_MLOOPCOL;
}

static bool dependsOnTime(struct ModifierData *md)
{
  // TODO: May depend on the OFX file
  return true;
}

static bool dependsOnNormals(struct ModifierData *md)
{
  // TODO: May depend on the OFX file (but harder to detect than time dependency -> add a user toggle)
  return true;
}

static void freeRuntimeData(void *runtime_data)
{
  if (runtime_data == NULL) {
    return;
  }
  printf("freeRuntimeData on pointer %p.\n", runtime_data);
  mfx_Modifier_free_runtime_data(runtime_data);
}

static void freeData(struct ModifierData *md)
{
  printf("OpenMeshEffectModifier: freeData.\n");
  OpenMeshEffectModifierData *fxmd = (OpenMeshEffectModifierData *)md;

  freeRuntimeData(md->runtime);
  md->runtime = NULL;

  if (fxmd->parameter_info) {
    MEM_freeN(fxmd->parameter_info);
  }

  if (fxmd->effect_info) {
    MEM_freeN(fxmd->effect_info);
  }
}

#if 0

def OPENMESHEFFECT(self, layout, _ob, md):
  layout.prop(md, "plugin_path")
  layout.separator()

  layout.prop(md, "effect_enum")
  layout.separator()

  PARAM_TYPE_INTEGER = 0
  PARAM_TYPE_INTEGER_2D = 1
  PARAM_TYPE_INTEGER_3D = 2
  PARAM_TYPE_DOUBLE = 3
  PARAM_TYPE_DOUBLE_2D = 4
  PARAM_TYPE_DOUBLE_3D = 5
  PARAM_TYPE_RGB = 6
  PARAM_TYPE_RGBA = 7
  PARAM_TYPE_BOOLEAN = 8
  PARAM_TYPE_CHOICE = 9
  PARAM_TYPE_STRING = 10
  PARAM_TYPE_CUSTOM = 11
  PARAM_TYPE_PUSH_BUTTON = 12
  PARAM_TYPE_GROUP = 13
  PARAM_TYPE_PAGE = 14

  for parm in md.parameter_info:
      row = layout.row(align=True)
      row.label(text=parm.label)
      if parm.type == PARAM_TYPE_INTEGER:
          row.prop(parm, "integer_value", text="")
      if parm.type == PARAM_TYPE_INTEGER_2D:
          row.prop(parm, "integer2d_value", text="")
      if parm.type == PARAM_TYPE_INTEGER_3D:
          row.prop(parm, "integer3d_value", text="")
      if parm.type == PARAM_TYPE_DOUBLE:
          row.prop(parm, "float_value", text="")
      if parm.type == PARAM_TYPE_DOUBLE_2D:
          row.prop(parm, "float2d_value", text="")
      if parm.type == PARAM_TYPE_DOUBLE_3D:
          row.prop(parm, "float3d_value", text="")
      if parm.type == PARAM_TYPE_RGB:
          row.prop(parm, "rgb_value", text="")
      if parm.type == PARAM_TYPE_RGBA:
          row.prop(parm, "rgba_value", text="")
      if parm.type == PARAM_TYPE_BOOLEAN:
          row.prop(parm, "boolean_value", text="")
      if parm.type == PARAM_TYPE_STRING:
          row.prop(parm, "string_value", text="")

#endif

static void panel_draw(const bContext *C, Panel *panel)
{
  uiLayout *row;
  uiLayout *layout = panel->layout;

  PointerRNA ob_ptr;
  PointerRNA *ptr = modifier_panel_get_property_pointers(panel, &ob_ptr);

  uiItemR(layout, ptr, "plugin_path", UI_ITEM_R_EXPAND, NULL, ICON_NONE);
  uiItemS(layout);

  uiItemR(layout, ptr, "effect_enum", 0, NULL, ICON_NONE);
  uiItemS(layout);

  char *label;
  int type;
  CollectionPropertyIterator iter;
  for (RNA_collection_begin(ptr, "parameter_info", &iter); iter.valid;
       RNA_property_collection_next(&iter)) {
    PointerRNA param_ptr = iter.ptr;
    row = uiLayoutRow(layout, true);

    label = RNA_string_get_alloc(&param_ptr, "label", NULL, 0);
    uiItemL(row, label, ICON_NONE);
    MEM_freeN(label);
    
    type = RNA_int_get(&param_ptr, "type");
    switch (type) {
      case PARAM_TYPE_INTEGER:
        uiItemR(row, ptr, "integer_value", 0, "", ICON_NONE);  // IFACE_("")?
        break;
      case PARAM_TYPE_INTEGER_2D:
        uiItemR(row, ptr, "integer2d_value", 0, "", ICON_NONE);
        break;
      case PARAM_TYPE_INTEGER_3D:
        uiItemR(row, ptr, "integer3d_value", 0, "", ICON_NONE);
        break;
      case PARAM_TYPE_DOUBLE:
        uiItemR(row, ptr, "float_value", 0, "", ICON_NONE);
        break;
      case PARAM_TYPE_DOUBLE_2D:
        uiItemR(row, ptr, "float2d_value", 0, "", ICON_NONE);
        break;
      case PARAM_TYPE_DOUBLE_3D:
        uiItemR(row, ptr, "float3d_value", 0, "", ICON_NONE);
        break;
      case PARAM_TYPE_RGB:
        uiItemR(row, ptr, "rgb_value", 0, "", ICON_NONE);
        break;
      case PARAM_TYPE_RGBA:
        uiItemR(row, ptr, "rgba_value", 0, "", ICON_NONE);
        break;
      case PARAM_TYPE_BOOLEAN:
        uiItemR(row, ptr, "boolean_value", 0, "", ICON_NONE);
        break;
      case PARAM_TYPE_STRING:
        uiItemR(row, ptr, "string_value", 0, "", ICON_NONE);
        break;
    }
  }

  modifier_panel_end(layout, ptr);
}

static void panelRegister(ARegionType *region_type)
{
  PanelType *panel_type = modifier_panel_register(region_type, eModifierType_OpenMeshEffect, panel_draw);
}

static void blendWrite(BlendWriter *writer, const ModifierData *md)
{
  const OpenMeshEffectModifierData *fxmd = (OpenMeshEffectModifierData *)md;

  BLO_write_struct_array_by_id(writer,
                               BLO_get_struct_id(writer, OpenMeshEffectParameterInfo),
                               fxmd->num_parameters,
                               fxmd->parameter_info);
}

static void blendRead(BlendDataReader *reader, ModifierData *md)
{
  OpenMeshEffectModifierData *fxmd = (OpenMeshEffectModifierData *)md;

  fxmd->parameter_info = BLO_read_data_address(reader, &fxmd->parameter_info);

  // Effect list will be reloaded from plugin
  fxmd->num_effects = 0;
  fxmd->effect_info = NULL;
}

ModifierTypeInfo modifierType_OpenMeshEffect = {
    /* name */ "Open Mesh Effect",
    /* structName */ "OpenMeshEffectModifierData",
    /* structSize */ sizeof(OpenMeshEffectModifierData),
    /* srna */ &RNA_OpenMeshEffectModifier,
    /* type */ eModifierTypeType_Constructive,
    /* flags */ eModifierTypeFlag_AcceptsMesh | eModifierTypeFlag_SupportsMapping |
        eModifierTypeFlag_SupportsEditmode | eModifierTypeFlag_EnableInEditmode,
    /* icon */ ICON_MOD_ARRAY,
    /* copyData */ copyData,
    /* deformVerts */ NULL,
    /* deformMatrices */ NULL,
    /* deformVertsEM */ NULL,
    /* deformMatricesEM */ NULL,
    /* modifyMesh */ modifyMesh,
    /* modifyHair */ NULL,
    /* modifyPointCloud */ NULL,
    /* modifyVolume */ NULL,
    /* initData */ initData,
    /* requiredDataMask */ requiredDataMask,
    /* freeData */ freeData,
    /* isDisabled */ NULL,
    /* updateDepsgraph */ NULL,
    /* dependsOnTime */ dependsOnTime,
    /* dependsOnNormals */ dependsOnNormals,
    /* foreachIDLink */ NULL,
    /* foreachTexLink */ NULL,
    /* freeRuntimeData */ freeRuntimeData,
    /* uiPanel */ panelRegister,
    /* blendWrite */ blendWrite,
    /* blendRead */ blendRead,
};
