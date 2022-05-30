#include "DNA_mesh_types.h"
#include "DNA_meshdata_types.h"

#include "BKE_attribute_math.hh"
#include "BKE_mesh.h"
#include "BKE_mesh_runtime.h"

#include "UI_interface.h"
#include "UI_resources.h"

#include "node_geometry_util.hh"

namespace blender::nodes::node_geo_pizza_cc {

static void node_declare(NodeDeclarationBuilder &b)
{
  b.add_input<decl::Float>(N_("Radius"))
      .default_value(1.0f)
      .min(0.0f)
      .subtype(PROP_DISTANCE)
      .description(N_("Size of the pizza"));
  b.add_output<decl::Geometry>("Mesh");
  b.add_output<decl::Bool>(N_("Base")).field_source();
  b.add_output<decl::Bool>(N_("Olives")).field_source();
}

static void node_init(bNodeTree *UNUSED(tree), bNode *node)
{
  NodeGeometryPizza *data = MEM_cnew<NodeGeometryPizza>(__func__);
  data->olive_count = 5;
  node->storage = data;
}

NODE_STORAGE_FUNCS(NodeGeometryPizza)  // To define node_storage()

static void node_update(bNodeTree *ntree, bNode *node)
{
  const NodeGeometryPizza &storage = node_storage(*node);

  bNodeSocket *out_socket_geometry = (bNodeSocket *)node->outputs.first;
  bNodeSocket *out_socket_base = out_socket_geometry->next;
  bNodeSocket *out_socket_olives = out_socket_base->next;

  // Stupid feature for the sake of the example: When there are too many
  // olives, we no longer output the fields!
  nodeSetSocketAvailability(ntree, out_socket_base, storage.olive_count < 25);
  nodeSetSocketAvailability(ntree, out_socket_olives, storage.olive_count < 25);
}

static void node_layout(uiLayout *layout, bContext *UNUSED(C), PointerRNA *ptr)
{
  uiLayoutSetPropSep(layout, true);
  uiLayoutSetPropDecorate(layout, false);
  uiItemR(layout, ptr, "olive_count", 0, "", ICON_NONE);
}

static Mesh *create_pizza_mesh(const int olive_count,
                               const float radius,
                               IndexRange &base_polys,
                               IndexRange &olives_polys)
{
  // (i) Compute element counts
  int vert_count = 32 + olive_count * 4;
  int edge_count = 32 + olive_count * 4;
  int corner_count = 32 + olive_count * 4;
  int face_count = 1 + olive_count;

  // (ii) Allocate memory
  Mesh *mesh = BKE_mesh_new_nomain(vert_count, edge_count, 0, corner_count, face_count);

  // (iii) Fill in element buffers
  MutableSpan<MVert> verts{mesh->mvert, mesh->totvert};
  MutableSpan<MLoop> loops{mesh->mloop, mesh->totloop};
  MutableSpan<MEdge> edges{mesh->medge, mesh->totedge};
  MutableSpan<MPoly> polys{mesh->mpoly, mesh->totpoly};
  base_polys = IndexRange{0, 1};
  olives_polys = IndexRange{1, olive_count};

  // (iii.a) Base
  const float angle_delta = 2 * M_PI / 32;
  for (const int i : IndexRange(32)) {
    // Vertex coordinates
    const float angle = i * angle_delta;
    copy_v3_v3(verts[i].co, float3(std::cos(angle) * radius, std::sin(angle) * radius, 0.0f));

    // Edge
    MEdge &edge = edges[i];
    edge.v1 = i;
    edge.v2 = (i + 1) % 32;
    edge.flag = ME_EDGEDRAW | ME_EDGERENDER;

    // Corner
    MLoop &loop = loops[i];
    loop.e = i;
    loop.v = i;
  }
  // Face
  MPoly &poly = polys[0];
  poly.loopstart = 0;
  poly.totloop = 32;

  // (iii.b) Olives
  const float angle_delta_olive = 2.0f * (M_PI / static_cast<float>(olive_count - 1));
  for (const int i : IndexRange(olive_count)) {
    const int offset = 32 + 4 * i;

    // Vertex coordinates
    float cx = 0, cy = 0;
    if (i > 0) {  // (the olive #0 is at the center)
      const float angle = (i - 1) * angle_delta_olive;
      cx = std::cos(angle) * radius / 2;
      cy = std::sin(angle) * radius / 2;
    }
    copy_v3_v3(verts[offset + 0].co, float3(cx + 0.05f, cy + 0.05f, 0.01f));
    copy_v3_v3(verts[offset + 1].co, float3(cx - 0.05f, cy + 0.05f, 0.01f));
    copy_v3_v3(verts[offset + 2].co, float3(cx - 0.05f, cy - 0.05f, 0.01f));
    copy_v3_v3(verts[offset + 3].co, float3(cx + 0.05f, cy - 0.05f, 0.01f));

    for (const int k : IndexRange(4)) {
      // Edge
      MEdge &edge = edges[offset + k];
      edge.v1 = offset + k;
      edge.v2 = offset + (k + 1) % 4;
      edge.flag = ME_EDGEDRAW | ME_EDGERENDER;

      // Corner
      MLoop &loop = loops[offset + k];
      loop.e = offset + k;
      loop.v = offset + k;
    }

    // Face
    MPoly &poly = polys[1 + i];
    poly.loopstart = offset;
    poly.totloop = 4;
  }

  BLI_assert(BKE_mesh_is_valid(mesh));
  return mesh;
}

static void node_geo_exec(GeoNodeExecParams params)
{
  // We first retrieve the property (olive count) and the input socket (radius)
  const NodeGeometryPizza &storage = node_storage(params.node());
  const int olive_count = storage.olive_count;
  const float radius = params.extract_input<float>("Radius");

  IndexRange base_polys, olives_polys;
  Mesh *mesh = create_pizza_mesh(olive_count, radius, base_polys, olives_polys);

  // We build a geometry set to wrap the mesh and set it as the output value
  GeometrySet output_geo = GeometrySet::create_with_mesh(mesh);
  params.set_output("Mesh", output_geo);

  MeshComponent &component = output_geo.get_component_for_write<MeshComponent>();
  if (params.output_is_required("Base")) {
    // Create the field from a range and a mesh component:
    StrongAnonymousAttributeID id("Base");
    OutputAttribute_Typed<bool> attribute = component.attribute_try_get_for_output_only<bool>(
        id.get(), ATTR_DOMAIN_FACE);
    attribute.as_span().slice(base_polys).fill(true);
    attribute.save();

    // Output this field in the Base output
    params.set_output("Base",
                      AnonymousAttributeFieldInput::Create<bool>(
                          std::move(id), params.attribute_producer_name()));
  }

  if (params.output_is_required("Olives")) {
    // [...] Idem for olives
    StrongAnonymousAttributeID id("Olives");
    OutputAttribute_Typed<bool> attribute = component.attribute_try_get_for_output_only<bool>(
        id.get(), ATTR_DOMAIN_FACE);
    attribute.as_span().slice(olives_polys).fill(true);
    attribute.save();

    // Output this field in the Base output
    params.set_output("Olives",
                      AnonymousAttributeFieldInput::Create<bool>(
                          std::move(id), params.attribute_producer_name()));
  }
}

}  // namespace blender::nodes::node_geo_pizza_cc

void register_node_type_geo_pizza()
{
  namespace file_ns = blender::nodes::node_geo_pizza_cc;

  static bNodeType ntype;
  geo_node_type_base(&ntype, GEO_NODE_PIZZA, "Pizza", NODE_CLASS_GEOMETRY);
  ntype.declare = file_ns::node_declare;
  node_type_init(&ntype, file_ns::node_init);
  node_type_update(&ntype, file_ns::node_update);
  ntype.geometry_node_execute = file_ns::node_geo_exec;
  node_type_storage(
      &ntype, "NodeGeometryPizza", node_free_standard_storage, node_copy_standard_storage);
  ntype.draw_buttons = file_ns::node_layout;
  nodeRegisterType(&ntype);
}
